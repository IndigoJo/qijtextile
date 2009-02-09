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

QijTextile::QijTextile( QString &_sourceText, QString _rel = "" )
{
  hlgn.setPattern( "(?:\<(?!>)|(?<!<)\>|\<\>|\=|[()]+(?! ))" );
  vlgn.setPattern( "[\-^~]" );
  clas.setPattern( "(?:\([^)]+\))" );
  lnge.setPattern( "(?:\[[^]]+\])" );
  styl.setPattern( "(?:\{[^}]+\})" );
  cspn.setPattern( "(?:\\\\\d+)" );
  rspn.setPattern( "(?:\/\d+)" );
  a.setPattern( QString( "(?:%1|%2)*" )
                .arg( hlgn.pattern() ).arg( vlgn.pattern() )
                );
  s.setPattern( QString( "(?:%1|%2)*" )
                .arg( cspn.pattern() ).arg( rspn.pattern() )
                );
  c.setPattern( QString( "(?:%1|%2|%3|%4)*" )
                .arg( clas.pattern() ).arg( styl.pattern() )
                .arg( lnge.pattern() ).arg( hlgn.pattern() )
                );

  pnct = "[\!i\"#\$%&\\'()\\*\\+,\\-\\./:;<=>\\?@\\[\\\\\\]\\^_`{\\|}\\~]";
  urlch = "[\\w\"$\-_.+!*\\'(),";\\/?:@=&%#{}|\\\\^~\\[\\]`]";

  urlSchemes << "http" << "https" << "ftp" << "mailto";
  btag << "bq" << "bc" << "notextile" << "pre" << "h[1-6]" << "fn\d+" << "p";

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

  sourceText = _sourceText;
  rel = _rel;
}

QString QijTextile::convert()
{
  if( encode ) {
    sourceText.replace( QRegExp( "&(?![#a-z0-9]+;)", Qt::CaseInsensitive ), "&#38;" );
    sourceText = incomingEntities( sourceText );
    sourceText.replace( "x%x%", "&#38;" );
    return sourceText;
  }
  else {
    if( !strict )
      sourceText = cleanWhiteSpace( sourceText );
    
    getRefs( sourceText );
    
    if( !lite )
      sourceText = block( sourceText );

    sourceText = retrieve( sourceText );
    
    sourceText.replace( QRegExp( "<br />(?!\n)" ), "<br />\n" );

    return sourceText;
  }
}

QString QijTextile::parseBlockAttributes( QString &in, QString element )
{
  QString style, klass, lang, colspan, rowspan, id, atts;
  QString matched, rv;
  QRegExp rx;
  
  if( !in.isEmpty() ) {
    matched = in;
    if( element == "td" ) {
      rx.setPattern( "\\\\{2}(\d+)" );
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
      style += rv.cap( 1 ).remove( QRegExp( ";$" ) ).append( ";" );
      matched.remove( rv.cap( 0 ) );
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

QString QijTextile::iAlign( QString &in )
{
  QMap<QString, QString> vals;

  vals["<"] = "left";
  vals["="] = "center";
  vals[">"] = "right";

  return vals.contains( in ) ?
    vals[in] : "";
}

QString QijTextile::hAlign( QString &in )
{
  QMap<QString, QString> vals;

  vals["<"] = "left";
  vals["="] = "center";
  vals[">"] = "right";
  vals["<>"] = "justify";

  return( vals.contains( in ) ?
            vals[in] : "" );
}

QString QijTextile::vAlign( QString &in )
{
  QMap<QString, QString> vals;

  vals["^"] = "top";
  vals["-"] = "middle";
  vals["~"] = "bottom";

  return vals.contains( in ) ?
    vals[in] : "";
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
    .arg( rx.cap( 1 ) )
    .arg( rx.cap( 2 ) )
    .arg( content )
    .arg( rx.cap( 4 ) );
}

QString QijTextile::block( QString &in )
{
  QString tre( btag.join( "|" ) );
  QStringList ourList( in.split( "\n\n" ) );
  QStringList params, out;
  QString tag( "p" );
  QString atts, cite, graf, ext;
  QString o1, o2, content, c2, c1;
  int anon, pos;
  
  QRegExp rx;
  rx.setPattern( QString( "^(%1)(%2%3)\\.(\\.?)(?::(\\S+))? (.*)$" )
                 .arg( tre ).arg( a ).arg( c ) );
  
  QStringList::iterator i;
  for( i = ourList.begin(); i != ourList.end(); ++i ) {
    params.clear();
    anon = 0;

    if( rx.indexIn( *i ) != -1 ) {
      if( !ext.isEmpty() )
        out[out.count()-1] += c1;

      tag  = rx.cap( 1 );
      atts = rx.cap( 2 );
      ext  = rx.cap( 3 );
      cite = rx.cap( 4 );
      graf = rx.cap( 5 );

      fBlock( rx.capturedTexts(), o1, o2, content, c2, c1 );
      
      if( ext.isEmpty() )
        *i = QString( "%1%2%3%4%5" )
          .arg( o1 ).arg( o2 ).arg( content ).arg( c2 ).arg( c1 );
      else
        // Block is extended; we'll close it in the next iteration
        *i = QString( "%1%2%3%4" )
          .arg( o1 ).arg( o2 ).arg( content ).arg( c2 );
    }
    else {
      anon = 1; // Anonymous block
      
      if( !ext.isEmpty() || !i->startsWith( ' ' ) ) {
        params << QString() << tag << atts << ext << cite << *i;
        fBlock( params, o1, o2, content, c2, c1 );
        if( rx.cap( 1 ) == "p" && !hasRawText( content ) )
          *i = content;
        else
          *i = QString( "%1%2%3" ).arg( o2 ).arg( content ).arg( c2 );
      }
      else
        *i = graf( *i );
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

void fBlock( QStringList &in, QString &o1, QString &o2, QString &content,
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
    fnid = (fn.at( rx.cap( 1 ) ).isEmpty()) ?
      rx.cap( 1 ) : fn.at( rx.cap( 1 ) );
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
      ctt = shelve( encode_html( ctt.remove( QRegExp( "\\n*$" ) ).append( "\n" ) ) );
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
          ctt = shelve( encode_html( ctt.remove( QRegExp( "\\n*$" ) ).append( "\n" ) ) );
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

  content = graf( ctt );
}

QString QijTextile::graf( QString &in )
{
  QString text( in );

  // Handle normal paragraph text
  if( lite ) {
    text = noTextile( text );
    text = code( text );
  }

  text = links( text );
  if( noImage )
    text = image( text );
  
  if( lite ) {
    text = lists( text );
    text = table( text );
  }

  text = span( text );
  text = footnoteRef( text );
  text = glyphs( text );
  
  return text.remove( QRegExp( "\\n*$" ) );
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

QString QijTextile::relURL( QString &u )
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

QString QijTextile::incomingEntities( QString &in )
{
  return in.replace( QRegExp( "&(?![#a-zA-Z0-9]+;)" ) );
}

QString QijTextile::fixEntities( QString &in )
{
  QString out( in );

  out.replace( "&gt;", ">" );
  out.replace( "&lt;", "<" );
  out.replace( "&amp;", "&" );

  return out;
}

QString QijTextile::glyphs( QString &in )
{
  QString out( in );
  QString pnc = "[[:punct:]]";

  out.replace( QRegExp( "\"\\z" ), "\" " );
  
  QList<QString> glyphReplace;
  glyphReplace << "(\\w)'(\\w)\"                           // apostrophe's
    << "(\\s)'(\\d+\\w?)\\b(?!')"                          // back in '88
    << "(\\S)'(?=\\s|'.$pnc.'|<|$)/"                       //  single closing
    << "'"                                                 //  single opening
    << "(\\S)\\"(?=\\s|'.$pnc.'|<|$)"                      //  double closing
    << "\""                                                //  double opening
    << "\\b([A-Z][A-Z0-9]{2,})\\b(?:[(]([^)]*)[)])"        //  3+ uppercase acronym
    << "\\b([A-Z][A-Z'\\-]+[A-Z])(?=[\\s.,\\)>])"          //  3+ uppercase
    << "\\b( )?\\.{3}"                                     //  ellipsis
    << "(\\s?)--(\\s?)"                                    //  em dash
    << "\\s-(?:\\s|$)"                                     //  en dash
    << "(\\d+)( ?)x( ?)(?=\\d+)"                           //  dimension sign
    << "\\b ?[([]TtMm[])]"                                 //  trademark
    << "\\b ?[([]Rr[])]"                                   //  registered
    << "\\b ?[([]Cc[])]";                                  //  copyright
 
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

QString QijTextile::blockLite( QString &in )
{
  btag = QStringList();
  btag << "bq" << "p";
  return block( QString( "%1\n\n" ).arg( in ) );
}

