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

QijTextile::QijTextile()
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
}
