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

#ifndef QIJTEXTILE_H
#define QIJTEXTILE_H

#include <QString>
#include <QStringList> // includes QList
#include <QHash>
#include <QUrl>
#include <QRegExp>
#include <QMap>

#define TXT_QUOTE_SINGLE_OPEN           "&#8216"
#define TXT_QUOTE_SINGLE_CLOSE          "&#8217"
#define TXT_QUOTE_DOUBLE_OPEN           "&#8220"
#define TXT_QUOTE_DOUBLE_CLOSE          "&#8221"
#define TXT_APOSTROPHE                  "&#8217"
#define TXT_PRIME                       "&#8242"
#define TXT_PRIME_DOUBLE                "&#8243"
#define TXT_ELLIPSIS                    "&#8230"
#define TXT_EMDASH                      "&#8212"
#define TXT_ENDASH                      "&#8211"
#define TXT_DIMENSION                   "&#215"
#define TXT_TRADEMARK                   "&#8482"
#define TXT_REGISTERED                  "&#174"
#define TXT_COPYRIGHT                   "&#169"

class QijTextile
{
 public:
  QijTextile( QString &, QString _rel = "" );
  QString convert( bool encode = false );
  QString convertRestricted( bool _lite = true, bool _noImage = true,
                             QString _rel = "" );
  QString toString();

  // Set the bools which were set in textileThis; by default, all are false
  void setRestricted( bool r ) { restricted = r; }
  void setNoImage( bool n )    { noImage = n; }
  void setStrict( bool s )     { strict = s; }
  void setLite( bool l )       { lite = l; }
  void setRel( QString &_rel ) { rel = _rel; }
  void setText( QString t )    { sourceText = t; }
  
  static QString textileThis( QString &, QString _rel = "" );
  static QString textileRestricted( QString &, QString _rel = "" );
  /* static QString substitute( QString &, QRegExp &, QString & );
  static QString substitute( QString &a, QString &b, QString &c ) {
    return substitute( a, QRegExp( b ), c ); } */

 private:
  QString sourceText, outText;
  bool restricted, noImage, strict, lite;
  QString hlgn, vlgn, clas, lnge, styl, cspn, rspn;
  QString a, s, c;
  QString pnct, urlch; // regexp fragments
  QString rel, hu;
  QStringList urlSchemes, btag;
  QHash<QString, QString> glyph;
  QMap<QString, QString> urlRefs;
  QMap<QString, QString> shelf;
  QMap<QString, QString> fn;
  typedef enum _method { Textile, Code, Pre, Special } Method;

  QString parseBlockAttributes( QString, QString element = "" );
  bool hasRawText( QString & );
  QString table( QString & );
  QString lists( QString & );
  QChar lT( QString & );
  QString doPBr( QString & );
  QString block( QString );
  void fBlock( QStringList &, QString &, QString &, QString &,
               QString &, QString & );
  //QString graf( QString & );
  QString graff( QString );
  QString span( QString & );
  QString fSpan( QStringList & );
  QString links( QString );
  QString relURL( QString & );
  QString getRefs( QString & );
  // QString refs( QStringList & );
  QString incomingEntities( QString );
  QString fixEntities( QString & );
  QString footnoteRef( QString );
  QString glyphs( QString );
  QString image( QString & );
  QString fImage( QStringList & );
  QString code( QString & );
  QString fCode( QStringList );
  QString fPre( QStringList );
  QString shelve( QString );
  QString retrieve( QString & );
  QString cleanWhiteSpace( QString & );
  QString doSpecial( QString &, QString, QString, Method meth = Special );
  QString fSpecial( QStringList );
  QString noTextile( QString & );
  QString fTextile( QStringList );
  QString encodeHtml( QString &, bool quotes = true );
  QString blockLite( QString & );
  QString iAlign( QString );
  QString hAlign( QString );
  QString vAlign( QString );
};

#endif

