/*
QijTextile - A port of the Textile text converter to C++ and Qt 4
Copyright (C) 2009 Matthew J Smith

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

#include "QijTextile.h"

#include <QtCore>

QijTextile::QijTextile( QString &_sourceText, QString _rel )
          : sourceText( _sourceText ), rel( _rel )
{
  hlgn = "(?:\\<(?!>)|(?<!<)\\>|\\<\\>|\\=|[()]+(?! ))";
  vlgn = "[\\-^~]";
  clas = "(?:\\([^)]+\\))";
  lnge = "(?:\\[[^]]+\\])";
  styl = "(?:\\{[^}]+\\})";
  cspn = "(?:\\\\\\d+)";
  rspn = "(?:/\\d+)";
  a = QString( "(?:%1|%2)*" ).arg( hlgn ).arg( vlgn );
  s = QString( "(?:%1|%2)*" ).arg( cspn ).arg( rspn );
  c = QString( "(?:%1|%2|%3|%4)*" )
    .arg( clas ).arg( styl ).arg( lnge ).arg( hlgn );

  pnct = "[\\!i\\\"#\\$%&\\'()\\*\\+,\\-\\./:;<=>\\?@\\[\\\\\\]\\^_`{\\|}\\~]";
  urlch = "[\\w\"$\\-_.+!*\\'(),\";\\/?:@=&%#{}|\\\\^~\\[\\]`]";

  urlSchemes << "http" << "https" << "ftp" << "mailto";
  btag << "bq" << "bc" << "notextile" << "pre" << "h[1-6]" << "fn\\d+" << "p";

  glyph["quote_single_open"] = TXT_QUOTE_SINGLE_OPEN;
  glyph["quote_single_close"] = TXT_QUOTE_SINGLE_CLOSE;
  glyph["quote_double_open"] = TXT_QUOTE_DOUBLE_OPEN;
  glyph["quote_double_close"] = TXT_QUOTE_DOUBLE_CLOSE;
  glyph["apostrophe"] = TXT_APOSTROPHE;
  glyph["prime"] = TXT_PRIME;
  glyph["prime_double"] = TXT_PRIME_DOUBLE;
  glyph["ellipsis"] = TXT_ELLIPSIS;
  glyph["emdash"] = TXT_EMDASH;
  glyph["endash"] = TXT_ENDASH;
  glyph["dimension"] = TXT_DIMENSION;
  glyph["trademark"] = TXT_TRADEMARK;
  glyph["registered"] = TXT_REGISTERED;
  glyph["copyright"] = TXT_COPYRIGHT;

  hu = "";

  restricted = false;
  lite = false;
  strict = false;
  noImage = false;

  //sourceText = _sourceText;
  //rel = _rel;
}

QString QijTextile::convert( bool encode )
{
  outText = sourceText;

  if( encode ) {
    outText.replace( QRegExp( "&(?![#a-z0-9]+;)", Qt::CaseInsensitive ), "&#38;" );
    outText = incomingEntities( sourceText );
    outText.replace( "x%x%", "&#38;" );
    return outText;
  }
  else {
    if( !strict )
      outText = cleanWhiteSpace( outText );
    
    getRefs( outText );
    qDebug() << "done getRefs";
    
    if( !lite )
      outText = block( outText );
    qDebug() << "done block";

    outText = retrieve( outText );
    qDebug() << "done retrieve";
    
    outText.replace( QRegExp( "<br />(?!\\n)" ), "<br />\\n" );

    return outText;
  }
}

QString QijTextile::textileThis( QString &text, QString _rel )
{
  QijTextile t( text, _rel );
  qDebug() << t.convert();
  return t.convert();
}

QString QijTextile::textileRestricted( QString &text, QString _rel )
{
  QijTextile t( text, _rel );
  return t.convertRestricted();
}

QString QijTextile::convertRestricted( bool _lite, bool _noImage,
                                       QString _rel )
{
  outText = sourceText;
  restricted = true;
  lite = _lite;
  noImage = _noImage;
  
  if( !rel.isEmpty() ) {
    rel = _rel;

    outText = encodeHtml( outText, false );
    outText = cleanWhiteSpace( outText );
    outText = getRefs( outText );

    if( lite ) 
      outText = blockLite( outText );
    else
      outText = block( outText );

    outText = retrieve( outText );

    outText.replace( "<br />", "br />\n" );
  }

  return outText;
}

QString QijTextile::parseBlockAttributes( QString in, QString element )
{
  QString style, klass, lang, colspan, rowspan, id, atts;
  QString matched, rv;
  QRegExp rx;
  
  if( !in.isEmpty() ) {
    matched = in;
    if( element == "td" ) {
      rx.setPattern( "\\\\{2}(\\d+)" );
      if( rx.indexIn( matched ) != -1 )
        colspan = rx.cap( 1 );
      rx.setPattern( "/(\\d+)" );
      if( rx.indexIn( matched ) != -1 )
        rowspan = rx.cap( 1 );
    }

    if( element == "td" || element == "tr" ) {
      rx.setPattern( QString( "(%1)" ).arg( vlgn ) );
      if( rx.indexIn( matched ) != -1 )
        style += QString( "vertical-align: %1;" ).arg( vAlign( rx.cap( 1 ) ) );
    }

    rx.setPattern( "\\{([^}]*)\\}" );
    if( rx.indexIn( matched ) != -1 ) {
      style += rx.cap( 1 ).remove( QRegExp( ";$" ) ).append( ";" );
      matched.remove( rx.cap( 0 ) );
    }

    rx.setPattern( "\\[([^]]+)\\]" );
    rx.setMinimal( true );
    if( rx.indexIn( matched ) != -1 ) {
      lang = rx.cap( 1 );
      matched.remove( rx.cap( 0 ) );
    }

    rx.setPattern( "\\(([^()]+)\\)" );
    if( rx.indexIn( matched ) != -1 ) {
      klass = rx.cap( 1 );
      matched.remove( rx.cap( 0 ) );
    }

    rx.setPattern( "([(]+)" );
    rx.setMinimal( false );
    if( rx.indexIn( matched ) != -1 ) {
      style += QString( "padding-left: %1em;" ).arg( rx.cap( 1 ).length() );
      matched.remove( rx.cap( 0 ) );
    }

    rx.setPattern( "([)]+)" );
    if( rx.indexIn( matched ) != -1 ) {
      style += QString( "padding-right: %1em;" ).arg( rx.cap( 1 ).length() );
      matched.remove( rx.cap( 0 ) );
    }

    rx.setPattern( QString( "(%1)" ).arg( hlgn ) );
    if( rx.indexIn( matched ) != -1 )
      style += QString( "text-align: %1;" ).arg( hAlign( rx.cap( 1 ) ) );
      
    rx.setPattern( "^(.*)#(.*)$" );
    if( rx.indexIn( klass ) != -1 ) {
      id = rx.cap( 2 );
      klass = rx.cap( 1 );
    }
    
    if( restricted )
      return ( lang.isEmpty() ? "" :
                 QString( " lang=\"%1\"" ).arg( lang ) );

    if( !style.isEmpty() )
      rv += QString( " style=\"%1\"" ).arg( style );
    if( !klass.isEmpty() )
      rv += QString( " class=\"%1\"" ).arg( klass );
    if( !lang.isEmpty() )
      rv += QString( " lang=\"%1\"" ).arg( lang );
    if( !id.isEmpty() )
      rv += QString( " id=\"%1\"" ).arg( id );
    if( !colspan.isEmpty() )
      rv += QString( " colspan=\"%1\"" ).arg( colspan );
    if( !rowspan.isEmpty() )
      rv += QString( " rowspan=\"%1\"" ).arg( rowspan );
    
    return rv;
  }
   
  return QString( "" );
} 

bool QijTextile::hasRawText( QString &in )
{
  // Checks whether text is not already enclosed with a block tag
  QString s = in.trimmed();
  QString r = s.remove( QRegExp( "<(p|blockquote|div|form|table|ul|ol|pre|h\\d)[^>]*?>.*</\\1>" ) ).trimmed();
  r.remove( QRegExp( "<(hr|br)[^>]*?/>" ) );
  r = r.trimmed();

  return !r.isEmpty();
}

QString QijTextile::table( QString &text )
{
  QString ourString( text );
  QString ratts, catts, ctyp;
  QStringList::iterator iter1, iter2;
  QStringList rmtch, cellsList;
  
  QRegExp rx1( QString( "^(?:table(_?{%1}{%2}{%3})\\. ?\\n)?^({%2}{%3}\\.? ?\\|.*\\|)\\n\\n" )
               .arg( s ).arg( a ).arg( c ) );
  QRegExp rx2( "\\|$" );
  rx2.setMinimal( true );
  QRegExp rx3( QString( "^(%1%2\\. )(.*)" ).arg( a ).arg( c ) );
  //rx3.setMinimal( true );
  QRegExp rx4( QString( "^(_?%1%2%3\\. )(.*)" ).arg( s ).arg( a ).arg( c ) );

  rx1.indexIn( ourString );
  QStringList matches = rx1.capturedTexts();

  QString tatts = parseBlockAttributes( matches[1] );

  QStringList rows = matches[2].split( rx2, QString::SkipEmptyParts );
  for( iter1 = rows.begin(); iter2 != rows.end(); ++iter1 ) {
    if( rx3.indexIn( iter1->section( QRegExp( "\\s*" ), 1 ) ) != -1 ) {
      ratts = parseBlockAttributes( rx3.cap( 1 ), "tr" );
      *iter1 = rx3.cap( 2 );
    }
    else
      ratts = "";
    
    cellsList = iter1->split( '|' );
    for( iter2 = cellsList.begin(); iter2 != cellsList.end(); ++iter2 ) {
      ctyp = "d";
      if( iter2->startsWith( '_' ) )
        ctyp = "h";
      if( rx4.indexIn( *iter2 ) != -1 ) {
        catts = parseBlockAttributes( rx4.cap( 1 ), "td" );
        *iter2 = rx4.cap( 2 );
      }
      else
        catts = "";
      
      *iter2 = graff( span( *iter2 ) );
  
      if( !iter2->trimmed().isEmpty() )
        cellsList.append( QString( "\t\t\t<t%1%2>%3</t%1>" )
                          .arg( ctyp ).arg( catts ).arg( *iter2 ) );
    }
    *iter1 = QString( "\t\t<tr%1>\n%2%3\t\t</tr>" )
      .arg( ratts ).arg( cellsList.join( "\n" ) )
      .arg( cellsList.count() ? "\n" : "" );
    cellsList = QStringList();
    catts = "";
  }
  ourString.replace( rx1.cap( 0 ),
                     QString( "\t<table%1>\n%2\n\t</table>\n\n" )
                     .arg( tatts )
                     .arg( rows.join( "\n" ) ) );
  return ourString;
}

QString QijTextile::lists( QString &in )
{
  QString nextLine, thisKey, returnValue;
  QString tl, nl, atts, content;
  QMap<QString, bool> lists;
  QStringList keys, out;
  QStringList::iterator iter;
  QString outString( in );

  QRegExp rx1( QString( "^([#*]+%1 .*)$(?![^#*])" ).arg( c ) );
  rx1.setMinimal( true );
  QRegExp rx2( QString( "^([#*]+)(%1%2) (.*)$" ).arg( a ).arg( c ) );
  QRegExp rx3( "^([#*]+)\\s.*" );
  
  rx1.indexIn( outString );
  QStringList text = rx1.cap( 0 ).split( '\n' );

  for( iter = text.begin(); iter != text.end(); ++iter ) {
    nextLine = *(iter+1);
    if( rx2.indexIn( *iter ) != -1 ) {
      tl = rx2.cap( 1 );
      atts = rx2.cap( 2 );
      content = rx3.cap( 3 );
      nl = "";
      if( rx3.indexIn( nextLine ) != -1 )
        nl = rx3.cap( 1 );
      if( !lists.contains( tl ) ) {
        lists[nl] = true;
        atts = parseBlockAttributes( atts );
        *iter = QString( "\t<%1l%2>\n\t\t<li>%3" )
          .arg( lT( tl ) )
          .arg( atts )
          .arg( graff( content ) );
      }
      else
        *iter = QString( "\t\t<li>%1" ).arg( graff( content ) );

      if( nl.length() <= tl.length() )
        *iter += "</li>";
      keys = lists.keys();
      QStringListIterator iter2( keys );
      while( iter2.hasPrevious() ) {
        thisKey = iter2.previous();
        if( thisKey.length() > nl.length() ) {
          *iter += ( QString( "\n\t</%1l>" ).arg( lT( thisKey ) ) );
          if( thisKey.length() > 1 )
            *iter += ( "</li>" );
          lists.remove( thisKey );
        }
      }
    }
    out.append( *iter );
  }

  outString.replace( rx1.cap( 0 ), out.join( "\n" ) );
  return outString;  
}

inline QChar QijTextile::lT( QString &in )
{
  return (QRegExp( "^#+" ).indexIn( in ) != -1) ? 'o' : 'u';
}

QString QijTextile::doPBr( QString &in )
{
  QRegExp rx1( "<(p)([^>]*?)>(.*)(</\\1>)" );
  QRegExp rx2( "(.+)(?<!<br>|<br />)\\n(?![#*\\s|])" );
  QStringList caps1, caps2;
  QString content, caps1_3, out;

  int a = rx1.indexIn( in );
  caps1 = rx1.capturedTexts();
  caps1_3 = caps1[3];
  a = rx2.indexIn( caps1_3 );
  caps2 = rx2.capturedTexts();
  content = caps1_3.replace( caps2[0], QString( "%1<br />" ).arg( caps2[1] ) );
      
  return QString( "<%1%2>%3%4" )
    .arg( rx1.cap( 1 ) )
    .arg( rx1.cap( 2 ) )
    .arg( content )
    .arg( rx1.cap( 4 ) );
}

QString QijTextile::block( QString in )
{
  QString tre( btag.join( "|" ) );
  QStringList ourList( in.split( "\n\n" ) );
  QStringList params, out, caps;
  QString tag( "p" );
  QString atts, cite, graf, ext;
  QString o1, o2, content, c2, c1;
  int anon;
  QString grafText;
  
  QRegExp rx;
  rx.setPattern( QString( "^(%1)(%2%3)\\.(\\.?)(?::(\\S+))? (.*)$" )
                 .arg( tre ).arg( a ).arg( c ) );
  
  QStringList::iterator i;
  for( i = ourList.begin(); i != ourList.end(); ++i ) {
    params.clear();
    anon = 0;

    qDebug() << "now testing regexp";
    if( rx.indexIn( *i ) != -1 ) {
      qDebug() << "tested the regexp (true)";
      if( !ext.isEmpty() )
        out[out.count()-1] += c1;

      tag  = rx.cap( 1 );
      atts = rx.cap( 2 );
      ext  = rx.cap( 3 );
      cite = rx.cap( 4 );
      graf = rx.cap( 5 );

      caps = rx.capturedTexts();
      fBlock( caps, o1, o2, content, c2, c1 );
      
      if( ext.isEmpty() )
        *i = QString( "%1%2%3%4%5" )
          .arg( o1 ).arg( o2 ).arg( content ).arg( c2 ).arg( c1 );
      else
        // Block is extended; we'll close it in the next iteration
        *i = QString( "%1%2%3%4" )
          .arg( o1 ).arg( o2 ).arg( content ).arg( c2 );
    }
    else {
      qDebug() << "tested the regexp (false)";
      anon = 1; // Anonymous block
      
      if( !ext.isEmpty() || !i->startsWith( ' ' ) ) {
        params << QString() << tag << atts << ext << cite << *i;
        qDebug() << "Loaded the params";
        fBlock( params, o1, o2, content, c2, c1 );
        qDebug() << "done fBlock";
        if( rx.cap( 1 ) == "p" && !hasRawText( content ) )
          *i = content;
        else
          *i = QString( "%1%2%3" ).arg( o2 ).arg( content ).arg( c2 );
      }
      else {
        grafText = *i;
        grafText = graff( grafText );
        *i = grafText;
      }
    }

    *i = doPBr( *i );
    i->replace( "<br>", "<br />" );
    
    if( anon && !ext.isEmpty() )
      out[out.count()-1] = *i;
    else
      out << *i;

    if( ext.isEmpty() ) {
      tag = "p";
      atts = "";
      cite = "";
      graf = "";
    }
  }
  if( !ext.isEmpty() )
    out.last() += c1;

  return out.join( "\n\n" );
}

void QijTextile::fBlock( QStringList &in, QString &o1, QString &o2, QString &content,
                         QString &c2, QString &c1 )
{
  QString fnid;
  QRegExp rx( "fn(\\d+)" );

  o1 = "";
  o2 = "";
  c2 = "";
  c1 = "";

  QString tag =  in[1];
  QString atts = parseBlockAttributes( in[2] );
  QString ext  = in[3];
  QString cite = in[4];
  QString ctt  = in[5];

  if( rx.indexIn( tag ) != -1 ) {
    tag = "p";
    fnid = (fn.value( rx.cap( 1 ) ).isEmpty()) ?
      rx.cap( 1 ) : fn.value( rx.cap( 1 ) );
    atts += QString( " id=\"%1\"" ).arg( fnid );
    if( !atts.contains( "class=" ) )
      atts += " class=\"footnote\"";
    ctt = QString( "<sup>%1</sup>%2" ).arg( rx.cap( 1 ) ).arg( ctt );
  }
 
  if( tag == "bq" ) {
    cite = checkRefs( cite );
    if( cite.isEmpty() )
      cite = QString( " cite=\"%1\"" ).arg( cite );
    else
      cite = "";
    o1 = QString( "\t<blockquote%1%2>\n" ).arg( cite ).arg( atts );
    o2 = QString( "\t\t<p%1>" ).arg( atts );
    c2 = "</p>";
    c1 = "\n\t</blockquote>";
  }
  else {
    if( tag == "bc" ) {
      o1 = QString( "<pre%1>" ).arg( atts );
      o2 = QString( "<code%1>" ).arg( atts );
      c2 = "</code>";
      c1 = "</pre>";
      ctt = shelve( encodeHtml( ctt.remove( QRegExp( "\\n*$" ) ).append( "\n" ) ) );
    }
    else {
      if( tag == "notextile" ) {
        ctt = shelve( ctt );
        o1 = "";
        o2 = "";
        c2 = "";
        c1 = "";
      }
      else {
        if( tag == "pre" ) {
          ctt = shelve( encodeHtml( ctt.remove( QRegExp( "\\n*$" ) ).append( "\n" ) ) );
          o1 = QString( "<pre%1>" ).arg( atts );
          o2 = "";
          c2 = "";
          c1 = "</pre>";
        }
        else {
          o2 = QString( "\t<%1%2>" ).arg( tag ).arg( atts );
          c2 = "</$tag>";
        }
      }
    }
  }

  content = graff( ctt );
}

QString QijTextile::graff( QString in )
{
  QString text( in );

  // Handle normal paragraph text
  if( lite ) {
    text = noTextile( text );
    text = code( text );
  }

  text = links( text );
  qDebug() << "done links test";
  if( noImage )
    text = image( text );
  
  if( lite ) {
    text = lists( text );
    text = table( text );
  }

  text = glyphs( footnoteRef( span( text ) ) ).remove( QRegExp( "\\n*$" ) );;
  return text;
}

QString QijTextile::span( QString &in )
{
  int i;
  QStringList qtags;
  qtags << "\\*\\*" << "\\*" << "\\?\\?"
        << "-" << "__" << "_"
        << "%" << "\\+" << "~" << "\\^";
  QString pnct( ".,\"'?!;:" );
  QString rxs;
  QString out( in );
  QString rxs_base = QString( "(?:^|(?<=[\\s>%1])|([{[]))"
                         "($f)(?!$f)"
                         "({%2})"
                         "(?::(\\S+))?"
                         "([^\\s$f]+|\\S[^$f\\n]*[^\\s$f\\n])"
                         "([%1]*)"
                         "$f"
                         "(?:$|([\\]}])|(?=[[:punct:]]{1,2}|\\s))" )
    .arg( pnct ).arg( c );
  QRegExp rx1;
  QStringList caps;
  
  Q_FOREACH( QString qtag, qtags ) {
    qDebug() << "starting loop";
    rxs = rxs_base;
    rx1 = QRegExp( rxs.replace( "$f", qtag ) );
    i = rx1.indexIn( out );
    caps = rx1.capturedTexts();
    out.replace( rx1, fSpan( caps ) );
  }
  
  qDebug() << "done span";
  return out;
}

QString QijTextile::fSpan( QStringList &in )
{
  QMap<QString, QString> qtags;
  qtags["*"] = "strong";
  qtags["**"] = "b";
  qtags["??"] = "cite";
  qtags["_"]  = "em";
  qtags["__"] = "i";
  qtags["-"]  = "del";
  qtags["%"] = "span";
  qtags["+"] = "ins";
  qtags["~"] = "sub";
  qtags["^"] = "sup";

  QString tag = qtags.value( in.value( 1 ) );
  QString atts = parseBlockAttributes( in.value( 2 ) );
  QString cite = in.value( 3 );
  QString content = in.value( 4 );
  QString end = in.value( 5 );
  qDebug() << "assigned strings";

  if( !cite.isEmpty() )
    atts.append( QString( "cite=\"%1\"" ).arg( cite ) );

  QString out = QString( "<%1%2>%3%4</%1>" )
    .arg( tag ).arg( atts ).arg( content ).arg( end );
  return out;
}

QString QijTextile::links( QString in )
{
  QRegExp rx( QString( "(?:^|(?<=[\\s>.%1\\(])|([{[]))" // $pre
            "\""                            // start
            "(%2)"           // $atts
            "([^\"]+)"                // $text
            "\\s?"
            "(?:\\(([^)]+)\\)(?=\"))?"        // $title
            "\":"
            "(%3+)"          // $url
            "(\\/)?"                       // $slash
            "([^\\w\\/;]*)"                // $post
            "(?:([\\]}])|(?=\\s|$|\\)))" )
              .arg( pnct ).arg( c ).arg( urlch ) );
  rx.setMinimal( true );
  rx.indexIn( in );
  
  //  $pre, $atts, $text, $title, $url, $slash, $post)

  QString pre   = rx.cap( 1 );
  QString atts  = rx.cap( 2 );
  QString text  = rx.cap( 3 );
  QString title = rx.cap( 4 );
  QString url   = rx.cap( 5 );
  QString slash = rx.cap( 6 );
  QString post  = rx.cap( 7 );

  url = checkRefs( url );

  atts = parseBlockAttributes( atts );
  if( !title.isEmpty() ) 
    atts += QString( " title=\"%1\"" ).arg( encodeHtml( title ) );

  text = glyphs( span( text ) );
  qDebug() << "done glyphs";
  url = relURL( url );
  QString urlSlash( url );
  url += slash;

  QString out = shelve( QString( "<a href=\"%1\" %2%3>%4</a>%5" )
                        .arg( encodeHtml( urlSlash ) )
                        .arg( atts )
                        .arg( rel )
                        .arg( text )
                        .arg( post ) );
  
  in.replace( rx.cap( 0 ), out );
  return in;
}
  
QString QijTextile::checkRefs( QString in )
{
  if( urlRefs.contains( in ) )
    return urlRefs.value( in );
  else
    return in;
}

QString QijTextile::getRefs( QString &in )
{
  int pos = 0;
  QString ourString( in );
  QStringList caps;
  QRegExp rv( "(?<=^|\\s)\[(.+)\\]((?:http://|/)\\S+)(?=\\s|$)" );
  rv.setMinimal( true );

  while( rv.indexIn( ourString, pos ) != -1 ) {
    caps = rv.capturedTexts();
    urlRefs[caps[1]] = caps[2];
    ourString.remove( caps[0] );
    pos += rv.matchedLength();
  }
  
  return ourString;
}

QString QijTextile::relURL( QString u )
{
  QUrl url( u );
  QString out( u );
  QRegExp rx( "^\\w" );

  if( ( url.scheme().isEmpty() || url.scheme() == "http" ) &&
      url.host().isEmpty() && (rx.indexIn( url.path() ) != -1) )
    out = QString( "%1%2" ).arg( hu ).arg( out );
  
  if( restricted && url.scheme().isEmpty() && 
      urlSchemes.contains( url.scheme() ) )
    return "#";
  
  return out;
}

QString QijTextile::image( QString &in )
{
  QRegExp rx( QString( "(?:[[{])?"            // pre
                       "\\!"                  // opening !
                       "(\\<|\\=|\\>)??"      // optional alignment atts
                       "(%1)"                 // optional style,class atts
                       "(?:\\. )?"            // optional dot-space
                       "([^\\s(!]+)"          // presume this is the src
                       "\\s?"                 // optional space
                       "(?:\\(([^\\)]+)\\))?" // optional title
                       "\\!"                  // closing
                       "(?::(\\S+))?"         // optional href
                       "(?:[\\]}]|(?=\\s|$))" // lookahead: space or end of string
            ).arg( c ) );
  rx.setMinimal( true );
  
  QString out( in );
  if( rx.indexIn( out ) != -1 ) {
    QStringList caps = rx.capturedTexts();
    out.replace( rx, fImage( caps ) );
  }
  return out;
}

QString QijTextile::fImage( QStringList &in )
{
  QString algn = in[1];
  QString atts = parseBlockAttributes( in[2] );
  QString url = in[3];

  if( !algn.isEmpty() )
    atts.append( QString( " align=\"%1\"" ).arg( iAlign( algn ) ) );

  if( !in.value( 4 ).isEmpty() ) {
    atts.append( QString( " title=\"%1\"" ).arg( in.value( 4 ) ) );
    atts.append( QString( " alt=\"%1\"" ).arg( in.value( 4 ) ) );
  }
  else
    atts.append( " alt=\"\"" );
  // Now get image size

  QString href = !in.value( 5 ).isEmpty() ?
    checkRefs( in.value( 5 ) ) : "";
  url = relURL( checkRefs( url ) );
  
  QStringList out;
  out << (href.isEmpty() ? "" : QString( "<a href=\"%1\">" ).arg( href ));
  out << QString( "<img src=\"%1\" %2 />" ).arg( url ).arg( atts );
  out << (href.isEmpty() ? "" : "</a>");
  return out.join( "" );
}

QString QijTextile::code( QString &in )
{
  QString out( in );
  out = doSpecial( out, "<code>", "</code>", Code );
  out = doSpecial( out, "@", "@", Code );
  out = doSpecial( out, "<pre>", "</pre>", Pre );

  return out;
}

QString QijTextile::incomingEntities( QString in )
{
  return in.replace( QRegExp( "&(?![#a-zA-Z0-9]+;)" ), "x%x%" );
}

QString QijTextile::fixEntities( QString &in )
{
  QString out( in );

  out.replace( "&gt;", ">" );
  out.replace( "&lt;", "<" );
  out.replace( "&amp;", "&" );

  return out;
}

QString QijTextile::footnoteRef( QString in )
{
  QString out( in );
  QRegExp rx( "\\b\\[([0-9]+)\\](\\s)?" );

  rx.indexIn( out );
  QStringList matches( rx.capturedTexts() );

  if( fn.value( matches[1] ).isEmpty() )
    fn[matches[1]] = QUuid::createUuid().toString();
  QString fnid( fn[matches[1]] );
  QString fnText( matches.value( 2 ).isEmpty() ? "" : matches.value( 2 ) );
  out.replace( matches[0],
               QString( "<sup class=\"footnote\"><a href=\"#fn%1\">%2</a></sup>%3" )
               .arg( fnid ).arg( matches[2] ).arg( fnText ) );
  return out;
}

QString QijTextile::glyphs( QString ourString )
{
  QStringList glyphReplace;
  QStringList out;
  //int j;
  QString pnc = "[[:punct:]]";

  ourString.replace( QRegExp( "\"\\z" ), "\" " );
  
  QList<QString> glyphSearchStrings;
  glyphSearchStrings << "(\\w)'(\\w)"                          //  apostrophe's
    << "(\\s)'(\\d+\\w?)\\b(?!')"                          //  back in '88
    << "(\\S)'(?=\\s|'.$pnc.'|<|$)/"                       //  single closing
    << "'"                                                 //  single opening
    << "(\\S)\\\"(?=\\s|'.$pnc.'|<|$)"                     //  double closing
    << "\""                                                //  double opening
    << "\\b([A-Z][A-Z0-9]{2,})\\b(?:[(]([^)]*)[)])"        //  3+ uppercase acronym
    << "\\b([A-Z][A-Z'\\-]+[A-Z])(?=[\\s.,\\)>])"          //  3+ uppercase
    << "\\b( )?\\.{3}"                                     //  ellipsis
    << "(\\s?)--(\\s?)"                                    //  em dash
    << "\\s-(?:\\s|$)"                                     //  en dash
    << "(\\d+)( ?)x( ?)(?=\\d+)"                           //  dimension sign
    << "/\\b ?[([]TtMm[])]/i"                              //  trademark
    << "/\\b ?[([]Rr[])]/i"                                //  registered
    << "/\\b ?[([]Cc[])]/i";                               //  copyright

  QList<QRegExp> glyphSearch;
  Q_FOREACH( QString s, glyphSearchStrings ) {
    glyphSearch << (s.endsWith( "/i" ) ?
      QRegExp( s.section( 1, s.length()-3 ), Qt::CaseInsensitive ) :
      QRegExp( s ));
  }

  glyphReplace << QString( "$1%1$2" ).arg( glyph["txt_apostrophe"] )
    << QString( "$1%1$2" ).arg( glyph["txt_apostrophe"] )
    << QString( "$1%1" ).arg( glyph["txt_quote_single_quote"] )
    << glyph["txt_quote_single_open"]
    << QString( "$1%1" ).arg( glyph["txt_quote_double_open"] )
    << glyph["txt_quote_double_open"]
    << "<acronym title=\"$2\">$1</acronym>"
    << "<span class=\"caps\">$1</span"
    << QString( "$1%1" ).arg( glyph["txt_ellipsis"] )
    << QString( "$1%1$2" ).arg( glyph["txt_emdash"] )
    << QString( " %1 " ).arg( glyph["txt_endash"] )
    << QString( "$1$2%1$3" ).arg( glyph["txt_dimension"] )
    << glyph["txt_trademark"]
    << glyph["txt_registered"]
    << glyph["txt_copyright"];


  QRegExp rx1( "(<.*?>)" );
  rx1.setMinimal( true );
  QRegExp rx2( "<.*>" );

  QStringList lines = ourString.split( rx1 );
  QStringList::iterator iter;
  QString rString;
  for( iter = lines.begin(); iter != lines.end(); ++iter ) {
    if( rx2.indexIn( *iter ) != -1 ) {
      for( int i = 0; i < glyphSearch.count(); ++i ) {
        if( glyphSearch[i].indexIn( *iter ) != -1 ) {
          rString = *iter;
          rString.replace( glyphSearch[i].cap( 0 ),
                      glyphReplace[i].replace( "$1", glyphSearch[i].cap( 1 ) )
                                     .replace( "$2", glyphSearch[i].cap( 2 ) )
                                     .replace( "$3", glyphSearch[i].cap( 3 ) ) );
          *iter = rString;
        }
      }
    }
    out += *iter;
  }

  return out.join( "" );
}

QString QijTextile::fCode( QStringList in )
{
  return QString( "%1<code>%2</code>%3" )
    .arg( in[1] )
    .arg( restricted ? shelve( in[2] ) : shelve ( encodeHtml( in[2] ) ) )
    .arg( in[3] );
}

QString QijTextile::fPre( QStringList in )
{
  return QString( "%1<pre>%2</pre>%3" )
    .arg( in[1] )
    .arg( restricted ? shelve( in[2] ) : shelve( encodeHtml( in[2] ) ) )
    .arg( in[3] );
}

QString QijTextile::shelve( QString val )
{
  QString uuid = QUuid::createUuid().toString();
  shelf[uuid] = val;
  return uuid;
}

QString QijTextile::retrieve( QString &in )
{
  QString out( in );
  QString old;
  QMap<QString, QString>::iterator iter;

  do {
    old = out;
    for( iter = shelf.begin(); iter != shelf.end(); ++iter ) {
      out.replace( iter.key(), iter.value() );
    }
  } while( old != out );

  return out;
}

QString QijTextile::cleanWhiteSpace( QString &in )
{
  QString out( in );

  out.replace( "\r\n", "\n" );
  out.replace( QRegExp( "\\n{3,}" ), "\n\n" );
  out.replace( QRegExp( "\\n *\\n" ), "\n\n" );
  out.replace( QRegExp( "\"$" ), "\" " );

  return out;
}

QString QijTextile::doSpecial( QString &text, QString start,
                               QString end, Method meth )
{
  QString out( text );
  QRegExp rx( QString( "(^|\\s|[[({>])%1(.*?)(\\s|$|[\\])}])?" )
              .arg( QRegExp::escape( start ) )
              .arg( QRegExp::escape( end ) ) );

  rx.indexIn( text );
  switch( meth ) {
    case Textile: out.replace( rx.cap( 0 ), fTextile( rx.capturedTexts() ) ); break;
    case Code:    out.replace( rx.cap( 0 ), fCode( rx.capturedTexts() ) );    break;
    case Pre:     out.replace( rx.cap( 0 ), fPre( rx.capturedTexts() ) );     break;
    case Special: out.replace( rx.cap( 0 ), fSpecial( rx.capturedTexts() ) ); break;
  }
  
  return out;
}

QString QijTextile::fSpecial( QStringList in )
{
  return QString( "%1%2%3" )
    .arg( in[1] )
    .arg( shelve( encodeHtml( in[2] ) ) )
    .arg( in[3] );
}

QString QijTextile::noTextile( QString &in )
{
  QString out( doSpecial( in, QString( "<textile>" ), QString( "</notextile>" ), Textile ) );
  return doSpecial( out, QString( "==" ), QString( "==" ), Textile );
}

QString QijTextile::fTextile( QStringList in )
{
  return QString( "%1%2%3" )
    .arg( in[1] )
    .arg( shelve( in[2] ) )
    .arg( in[3] );
}

QString QijTextile::encodeHtml( QString &in, bool quotes )
{
  QMap<QChar, QString> symbols;
  symbols['&'] = "&#38;";
  symbols['<'] = "&#60;";
  symbols['>'] = "#&62;";

  if( quotes ) {
    symbols['\''] = "&#39";
    symbols['"'] = "&#34";
  }

  QString rv( in );
  Q_FOREACH( QChar ch, symbols.keys() )
    rv.replace( ch, symbols[ch] );
  return rv;
}

QString QijTextile::blockLite( QString &in )
{
  btag = QStringList();
  btag << "bq" << "p";
  return block( QString( "%1\n\n" ).arg( in ) );
}

QString QijTextile::iAlign( QString in )
{
  QMap<QString, QString> vals;

  vals["<"] = "left";
  vals["="] = "center";
  vals[">"] = "right";

  return vals.contains( in ) ?
    vals[in] : "";
}

QString QijTextile::hAlign( QString in )
{
  QMap<QString, QString> vals;

  vals["<"] = "left";
  vals["="] = "center";
  vals[">"] = "right";
  vals["<>"] = "justify";

  return( vals.contains( in ) ?
            vals[in] : "" );
}

QString QijTextile::vAlign( QString in )
{
  QMap<QString, QString> vals;

  vals["^"] = "top";
  vals["-"] = "middle";
  vals["~"] = "bottom";

  return vals.contains( in ) ?
    vals[in] : "";
}

