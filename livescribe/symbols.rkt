#lang racket/base

(provide symbol-table)

;;; HTML entity translation table
;;;
;;; These were taken from:
;;; https://en.wikipedia.org/wiki/List_of_XML_and_HTML_character_entity_references
;;; http://www.w3schools.com/tags/ref_entities.asp

(define symbol-table
  #hash(("&quot;"    . #\") ; quotation mark (= APL quote)
        ("&amp;"     . #\&) ; ampersand
        ("&apos;"    . #\') ; apostrophe (= apostrophe-quote)
        ("&lt;"      . #\<) ; less-than sign
        ("&gt;"      . #\>) ; greater-than sign
        ("&nbsp;"    . #\ ) ; no-break space (= non-breaking space)
        ("&iexcl;"   . #\¡) ; inverted exclamation mark
        ("&cent;"    . #\¢) ; cent sign
        ("&pound;"   . #\£) ; pound sign
        ("&curren;"  . #\¤) ; currency sign
        ("&yen;"     . #\¥) ; yen sign (= yuan sign)
        ("&brvbar;"  . #\¦) ; broken bar (= broken vertical bar)
        ("&sect;"    . #\§) ; section sign
        ("&uml;"     . #\¨) ; diaeresis (= spacing diaeresis)
        ("&copy;"    . #\©) ; copyright symbol
        ("&ordf;"    . #\ª) ; feminine ordinal indicator
        ("&laquo;"   . #\«) ; left-pointing double angle quotation mark (= left pointing guillemet)
        ("&not;"     . #\¬) ; not sign
        ("&shy;"     . #\ ) ; soft hyphen (= discretionary hyphen)
        ("&reg;"     . #\®) ; registered sign ( = registered trademark symbol)
        ("&macr;"    . #\¯) ; macron (= spacing macron = overline = APL overbar)
        ("&deg;"     . #\°) ; degree symbol
        ("&plusmn;"  . #\±) ; plus-minus sign (= plus-or-minus sign)
        ("&sup2;"    . #\²) ; superscript two (= superscript digit two = squared)
        ("&sup3;"    . #\³) ; superscript three (= superscript digit three = cubed)
        ("&acute;"   . #\´) ; acute accent (= spacing acute)
        ("&micro;"   . #\µ) ; micro sign
        ("&para;"    . #\¶) ; pilcrow sign ( = paragraph sign)
        ("&middot;"  . #\·) ; middle dot (= Georgian comma = Greek middle dot)
        ("&cedil;"   . #\¸) ; cedilla (= spacing cedilla)
        ("&sup1;"    . #\¹) ; superscript one (= superscript digit one)
        ("&ordm;"    . #\º) ; masculine ordinal indicator
        ("&raquo;"   . #\») ; right-pointing double angle quotation mark (= right pointing guillemet)
        ("&frac14;"  . #\¼) ; vulgar fraction one quarter (= fraction one quarter)
        ("&frac12;"  . #\½) ; vulgar fraction one half (= fraction one half)
        ("&frac34;"  . #\¾) ; vulgar fraction three quarters (= fraction three quarters)
        ("&iquest;"  . #\¿) ; inverted question mark (= turned question mark)
        ("&Agrave;"  . #\À) ; Latin capital letter A with grave accent (= Latin capital letter A grave)
        ("&Aacute;"  . #\Á) ; Latin capital letter A with acute accent
        ("&Acirc;"   . #\Â) ; Latin capital letter A with circumflex
        ("&Atilde;"  . #\Ã) ; Latin capital letter A with tilde
        ("&Auml;"    . #\Ä) ; Latin capital letter A with diaeresis
        ("&Aring;"   . #\Å) ; Latin capital letter A with ring above (= Latin capital letter A ring)
        ("&AElig;"   . #\Æ) ; Latin capital letter AE (= Latin capital ligature AE)
        ("&Ccedil;"  . #\Ç) ; Latin capital letter C with cedilla
        ("&Egrave;"  . #\È) ; Latin capital letter E with grave accent
        ("&Eacute;"  . #\É) ; Latin capital letter E with acute accent
        ("&Ecirc;"   . #\Ê) ; Latin capital letter E with circumflex
        ("&Euml;"    . #\Ë) ; Latin capital letter E with diaeresis
        ("&Igrave;"  . #\Ì) ; Latin capital letter I with grave accent
        ("&Iacute;"  . #\Í) ; Latin capital letter I with acute accent
        ("&Icirc;"   . #\Î) ; Latin capital letter I with circumflex
        ("&Iuml;"    . #\Ï) ; Latin capital letter I with diaeresis
        ("&ETH;"     . #\Ð) ; Latin capital letter Eth
        ("&Ntilde;"  . #\Ñ) ; Latin capital letter N with tilde
        ("&Ograve;"  . #\Ò) ; Latin capital letter O with grave accent
        ("&Oacute;"  . #\Ó) ; Latin capital letter O with acute accent
        ("&Ocirc;"   . #\Ô) ; Latin capital letter O with circumflex
        ("&Otilde;"  . #\Õ) ; Latin capital letter O with tilde
        ("&Ouml;"    . #\Ö) ; Latin capital letter O with diaeresis
        ("&times;"   . #\×) ; multiplication sign
        ("&Oslash;"  . #\Ø) ; Latin capital letter O with stroke (= Latin capital letter O slash)
        ("&Ugrave;"  . #\Ù) ; Latin capital letter U with grave accent
        ("&Uacute;"  . #\Ú) ; Latin capital letter U with acute accent
        ("&Ucirc;"   . #\Û) ; Latin capital letter U with circumflex
        ("&Uuml;"    . #\Ü) ; Latin capital letter U with diaeresis
        ("&Yacute;"  . #\Ý) ; Latin capital letter Y with acute accent
        ("&THORN;"   . #\Þ) ; Latin capital letter THORN
        ("&szlig;"   . #\ß) ; Latin small letter sharp s (= ess-zed)
        ("&agrave;"  . #\à) ; Latin small letter a with grave accent
        ("&aacute;"  . #\á) ; Latin small letter a with acute accent
        ("&acirc;"   . #\â) ; Latin small letter a with circumflex
        ("&atilde;"  . #\ã) ; Latin small letter a with tilde
        ("&auml;"    . #\ä) ; Latin small letter a with diaeresis
        ("&aring;"   . #\å) ; Latin small letter a with ring above
        ("&aelig;"   . #\æ) ; Latin small letter ae (= Latin small ligature ae)
        ("&ccedil;"  . #\ç) ; Latin small letter c with cedilla
        ("&egrave;"  . #\è) ; Latin small letter e with grave accent
        ("&eacute;"  . #\é) ; Latin small letter e with acute accent
        ("&ecirc;"   . #\ê) ; Latin small letter e with circumflex
        ("&euml;"    . #\ë) ; Latin small letter e with diaeresis
        ("&igrave;"  . #\ì) ; Latin small letter i with grave accent
        ("&iacute;"  . #\í) ; Latin small letter i with acute accent
        ("&icirc;"   . #\î) ; Latin small letter i with circumflex
        ("&iuml;"    . #\ï) ; Latin small letter i with diaeresis
        ("&eth;"     . #\ð) ; Latin small letter eth
        ("&ntilde;"  . #\ñ) ; Latin small letter n with tilde
        ("&ograve;"  . #\ò) ; Latin small letter o with grave accent
        ("&oacute;"  . #\ó) ; Latin small letter o with acute accent
        ("&ocirc;"   . #\ô) ; Latin small letter o with circumflex
        ("&otilde;"  . #\õ) ; Latin small letter o with tilde
        ("&ouml;"    . #\ö) ; Latin small letter o with diaeresis
        ("&divide;"  . #\÷) ; division sign (= obelus)
        ("&oslash;"  . #\ø) ; Latin small letter o with stroke (= Latin small letter o slash)
        ("&ugrave;"  . #\ù) ; Latin small letter u with grave accent
        ("&uacute;"  . #\ú) ; Latin small letter u with acute accent
        ("&ucirc;"   . #\û) ; Latin small letter u with circumflex
        ("&uuml;"    . #\ü) ; Latin small letter u with diaeresis
        ("&yacute;"  . #\ý) ; Latin small letter y with acute accent
        ("&thorn;"   . #\þ) ; Latin small letter thorn
        ("&yuml;"    . #\ÿ) ; Latin small letter y with diaeresis
        ("&OElig;"   . #\Œ) ; Latin capital ligature oe
        ("&oelig;"   . #\œ) ; Latin small ligature oe
        ("&Scaron;"  . #\Š) ; Latin capital letter s with caron
        ("&scaron;"  . #\š) ; Latin small letter s with caron
        ("&Yuml;"    . #\Ÿ) ; Latin capital letter y with diaeresis
        ("&fnof;"    . #\ƒ) ; Latin small letter f with hook (= function = florin)
        ("&circ;"    . #\ˆ) ; modifier letter circumflex accent
        ("&tilde;"   . #\˜) ; small tilde
        ("&Alpha;"   . #\Α) ; Greek capital letter Alpha
        ("&Beta;"    . #\Β) ; Greek capital letter Beta
        ("&Gamma;"   . #\Γ) ; Greek capital letter Gamma
        ("&Delta;"   . #\Δ) ; Greek capital letter Delta
        ("&Epsilon;" . #\Ε) ; Greek capital letter Epsilon
        ("&Zeta;"    . #\Ζ) ; Greek capital letter Zeta
        ("&Eta;"     . #\Η) ; Greek capital letter Eta
        ("&Theta;"   . #\Θ) ; Greek capital letter Theta
        ("&Iota;"    . #\Ι) ; Greek capital letter Iota
        ("&Kappa;"   . #\Κ) ; Greek capital letter Kappa
        ("&Lambda;"  . #\Λ) ; Greek capital letter Lambda
        ("&Mu;"      . #\Μ) ; Greek capital letter Mu
        ("&Nu;"      . #\Ν) ; Greek capital letter Nu
        ("&Xi;"      . #\Ξ) ; Greek capital letter Xi
        ("&Omicron;" . #\Ο) ; Greek capital letter Omicron
        ("&Pi;"      . #\Π) ; Greek capital letter Pi
        ("&Rho;"     . #\Ρ) ; Greek capital letter Rho
        ("&Sigma;"   . #\Σ) ; Greek capital letter Sigma
        ("&Tau;"     . #\Τ) ; Greek capital letter Tau
        ("&Upsilon;" . #\Υ) ; Greek capital letter Upsilon
        ("&Phi;"     . #\Φ) ; Greek capital letter Phi
        ("&Chi;"     . #\Χ) ; Greek capital letter Chi
        ("&Psi;"     . #\Ψ) ; Greek capital letter Psi
        ("&Omega;"   . #\Ω) ; Greek capital letter Omega
        ("&alpha;"   . #\α) ; Greek small letter alpha
        ("&beta;"    . #\β) ; Greek small letter beta
        ("&gamma;"   . #\γ) ; Greek small letter gamma
        ("&delta;"   . #\δ) ; Greek small letter delta
        ("&epsilon;" . #\ε) ; Greek small letter epsilon
        ("&zeta;"    . #\ζ) ; Greek small letter zeta
        ("&eta;"     . #\η) ; Greek small letter eta
        ("&theta;"   . #\θ) ; Greek small letter theta
        ("&iota;"    . #\ι) ; Greek small letter iota
        ("&kappa;"   . #\κ) ; Greek small letter kappa
        ("&lambda;"  . #\λ) ; Greek small letter lambda
        ("&mu;"      . #\μ) ; Greek small letter mu
        ("&nu;"      . #\ν) ; Greek small letter nu
        ("&xi;"      . #\ξ) ; Greek small letter xi
        ("&omicron;" . #\ο) ; Greek small letter omicron
        ("&pi;"      . #\π) ; Greek small letter pi
        ("&rho;"     . #\ρ) ; Greek small letter rho
        ("&sigmaf;"  . #\ς) ; Greek small letter final sigma
        ("&sigma;"   . #\σ) ; Greek small letter sigma
        ("&tau;"     . #\τ) ; Greek small letter tau
        ("&upsilon;" . #\υ) ; Greek small letter upsilon
        ("&phi;"     . #\φ) ; Greek small letter phi
        ("&chi;"     . #\χ) ; Greek small letter chi
        ("&psi;"     . #\ψ) ; Greek small letter psi
        ("&omega;"   . #\ω) ; Greek small letter omega
        ("&thetasym;". #\ϑ) ; Greek theta symbol
        ("&upsih;"   . #\ϒ) ; Greek Upsilon with hook symbol
        ("&piv;"     . #\ϖ) ; reek pi symbol
        ("&ensp;"    . #\ ) ; en space
        ("&emsp;"    . #\ ) ; em space
        ("&thinsp;"  . #\ ) ; thin space
        ("&zwnj;"    . #\ ) ; zero-width non-joiner
        ("&zwj;"     . #\ ) ; zero-width joiner
        ("&lrm;"     . #\ ) ; left-to-right mark
        ("&rlm;"     . #\ ) ; right-to-left mark
        ("&ndash;"   . #\–) ; en dash
        ("&mdash;"   . #\—) ; em dash
        ("&lsquo;"   . #\‘) ; left single quotation mark
        ("&rsquo;"   . #\’) ; right single quotation mark
        ("&sbquo;"   . #\‚) ; single low-9 quotation mark
        ("&ldquo;"   . #\“) ; left double quotation mark
        ("&rdquo;"   . #\”) ; right double quotation mark
        ("&bdquo;"   . #\„) ; double low-9 quotation mark
        ("&dagger;"  . #\†) ; dagger, obelisk
        ("&Dagger;"  . #\‡) ; double dagger, double obelisk
        ("&bull;"    . #\•) ; bullet (= black small circle)[
        ("&hellip;"  . #\…) ; horizontal ellipsis (= three dot leader)
        ("&permil;"  . #\‰) ; per mille sign
        ("&prime;"   . #\′) ; prime (= minutes = feet)
        ("&Prime;"   . #\″) ; double prime (= seconds = inches)
        ("&lsaquo;"  . #\‹) ; single left-pointing angle quotation mark
        ("&rsaquo;"  . #\›) ; single right-pointing angle quotation mark
        ("&oline;"   . #\‾) ; overline (= spacing overscore)
        ("&frasl;"   . #\⁄) ; fraction slash (= solidus)
        ("&euro;"    . #\€) ; euro sign
        ("&image;"   . #\ℑ) ; black-letter capital I (= imaginary part)
        ("&weierp;"  . #\℘) ; script capital P (= power set = Weierstrass p)
        ("&real;"    . #\ℜ) ; black-letter capital R (= real part symbol)
        ("&trade;"   . #\™) ; trademark symbol
        ("&alefsym;" . #\ℵ) ; alef symbol (= first transfinite cardinal)
        ("&larr;"    . #\←) ; leftwards arrow
        ("&uarr;"    . #\↑) ; upwards arrow
        ("&rarr;"    . #\→) ; rightwards arrow
        ("&darr;"    . #\↓) ; downwards arrow
        ("&harr;"    . #\↔) ; left right arrow
        ("&crarr;"   . #\↵) ; downwards arrow with corner leftwards (= carriage return)
        ("&lArr;"    . #\⇐) ; leftwards double arrow
        ("&uArr;"    . #\⇑) ; upwards double arrow
        ("&rArr;"    . #\⇒) ; rightwards double arrow
        ("&dArr;"    . #\⇓) ; downwards double arrow
        ("&hArr;"    . #\⇔) ; left right double arrow
        ("&forall;"  . #\∀) ; for all
        ("&part;"    . #\∂) ; partial differential
        ("&exist;"   . #\∃) ; there exists
        ("&empty;"   . #\∅) ; empty set (= null set = diameter)
        ("&nabla;"   . #\∇) ; nabla (= backward difference)
        ("&isin;"    . #\∈) ; element of
        ("&notin;"   . #\∉) ; not an element of
        ("&ni;"      . #\∋) ; contains as member
        ("&prod;"    . #\∏) ; n-ary product (= product sign)
        ("&sum;"     . #\∑) ; n-ary summation
        ("&minus;"   . #\−) ; minus sign
        ("&lowast;"  . #\∗) ; asterisk operator
        ("&radic;"   . #\√) ; square root (= radical sign)
        ("&prop;"    . #\∝) ; proportional to
        ("&infin;"   . #\∞) ; infinity
        ("&ang;"     . #\∠) ; angle
        ("&and;"     . #\∧) ; logical and (= wedge)
        ("&or;"      . #\∨) ; logical or (= vee)
        ("&cap;"     . #\∩) ; intersection (= cap)
        ("&cup;"     . #\∪) ; union (= cup)
        ("&int;"     . #\∫) ; integral
        ("&there4;"  . #\∴) ; therefore sign
        ("&sim;"     . #\∼) ; tilde operator (= varies with = similar to)
        ("&cong;"    . #\≅) ; congruent to
        ("&asymp;"   . #\≈) ; almost equal to (= asymptotic to)
        ("&ne;"      . #\≠) ; not equal to
        ("&equiv;"   . #\≡) ; identical to; sometimes used for 'equivalent to'
        ("&le;"      . #\≤) ; less-than or equal to
        ("&ge;"      . #\≥) ; greater-than or equal to
        ("&sub;"     . #\⊂) ; subset of
        ("&sup;"     . #\⊃) ; superset of
        ("&nsub;"    . #\⊄) ; not a subset of
        ("&sube;"    . #\⊆) ; subset of or equal to
        ("&supe;"    . #\⊇) ; superset of or equal to
        ("&oplus;"   . #\⊕) ; circled plus (= direct sum)
        ("&otimes;"  . #\⊗) ; circled times (= vector product)
        ("&perp;"    . #\⊥) ; up tack (= orthogonal to = perpendicular)
        ("&sdot;"    . #\⋅) ; dot operator
        ("&vellip;"  . #\⋮) ; vertical ellipsis
        ("&lceil;"   . #\⌈) ; left ceiling (= APL upstile)
        ("&rceil;"   . #\⌉) ; right ceiling
        ("&lfloor;"  . #\⌊) ; left floor (= APL downstile)
        ("&rfloor;"  . #\⌋) ; right floor
        ("&lang;"    . #\〈) ; left-pointing angle bracket (= bra)
        ("&rang;"    . #\〉) ; right-pointing angle bracket (= ket)
        ("&loz;"     . #\◊) ; lozenge
        ("&spades;"  . #\♠) ; black spade suit
        ("&clubs;"   . #\♣) ; black club suit (= shamrock)
        ("&hearts;"  . #\♥) ; black heart suit (= valentine)
        ("&diams;"   . #\♦) ; black diamond suit
        ("&#34;"     . #\") ; quotation mark
        ("&#39;"     . #\') ; apostrophe
        ("&#38;"     . #\&) ; ampersand
        ("&#60;"     . #\<) ; less-than
        ("&#62;"     . #\>) ; greater-than
        ("&#160;"    . #\ ) ; non-breaking space
        ("&#161;"    . #\¡) ; inverted exclamation mark
        ("&#162;"    . #\¢) ; cent
        ("&#163;"    . #\£) ; pound
        ("&#164;"    . #\¤) ; currency
        ("&#165;"    . #\¥) ; yen
        ("&#166;"    . #\¦) ; broken vertical bar
        ("&#167;"    . #\§) ; section
        ("&#168;"    . #\¨) ; spacing diaeresis
        ("&#169;"    . #\©) ; copyright
        ("&#170;"    . #\ª) ; feminine ordinal indicator
        ("&#171;"    . #\«) ; angle quotation mark (left)
        ("&#172;"    . #\¬) ; negation
        ("&#173;"    . #\ ) ; soft hyphen
        ("&#174;"    . #\®) ; registered trademark
        ("&#175;"    . #\¯) ; spacing macron
        ("&#176;"    . #\°) ; degree
        ("&#177;"    . #\±) ; plus-or-minus
        ("&#178;"    . #\²) ; superscript 2
        ("&#179;"    . #\³) ; superscript 3
        ("&#180;"    . #\´) ; spacing acute
        ("&#181;"    . #\µ) ; micro
        ("&#182;"    . #\¶) ; paragraph
        ("&#183;"    . #\·) ; middle dot
        ("&#184;"    . #\¸) ; spacing cedilla
        ("&#185;"    . #\¹) ; superscript 1
        ("&#186;"    . #\º) ; masculine ordinal indicator
        ("&#187;"    . #\») ; angle quotation mark (right)
        ("&#188;"    . #\¼) ; fraction 1/4
        ("&#189;"    . #\½) ; fraction 1/2
        ("&#190;"    . #\¾) ; fraction 3/4
        ("&#191;"    . #\¿) ; inverted question mark
        ("&#215;"    . #\×) ; multiplication
        ("&#247;"    . #\÷) ; division
        ("&#192;"    . #\À) ; capital a, grave accent
        ("&#193;"    . #\Á) ; capital a, acute accent
        ("&#194;"    . #\Â) ; capital a, circumflex accent
        ("&#195;"    . #\Ã) ; capital a, tilde
        ("&#196;"    . #\Ä) ; capital a, umlaut mark
        ("&#197;"    . #\Å) ; capital a, ring
        ("&#198;"    . #\Æ) ; capital ae
        ("&#199;"    . #\Ç) ; capital c, cedilla
        ("&#200;"    . #\È) ; capital e, grave accent
        ("&#201;"    . #\É) ; capital e, acute accent
        ("&#202;"    . #\Ê) ; capital e, circumflex accent
        ("&#203;"    . #\Ë) ; capital e, umlaut mark
        ("&#204;"    . #\Ì) ; capital i, grave accent
        ("&#205;"    . #\Í) ; capital i, acute accent
        ("&#206;"    . #\Î) ; capital i, circumflex accent
        ("&#207;"    . #\Ï) ; capital i, umlaut mark
        ("&#208;"    . #\Ð) ; capital eth, Icelandic
        ("&#209;"    . #\Ñ) ; capital n, tilde
        ("&#210;"    . #\Ò) ; capital o, grave accent
        ("&#211;"    . #\Ó) ; capital o, acute accent
        ("&#212;"    . #\Ô) ; capital o, circumflex accent
        ("&#213;"    . #\Õ) ; capital o, tilde
        ("&#214;"    . #\Ö) ; capital o, umlaut mark
        ("&#216;"    . #\Ø) ; capital o, slash
        ("&#217;"    . #\Ù) ; capital u, grave accent
        ("&#218;"    . #\Ú) ; capital u, acute accent
        ("&#219;"    . #\Û) ; capital u, circumflex accent
        ("&#220;"    . #\Ü) ; capital u, umlaut mark
        ("&#221;"    . #\Ý) ; capital y, acute accent
        ("&#222;"    . #\Þ) ; capital THORN, Icelandic
        ("&#223;"    . #\ß) ; small sharp s, German
        ("&#224;"    . #\à) ; small a, grave accent
        ("&#225;"    . #\á) ; small a, acute accent
        ("&#226;"    . #\â) ; small a, circumflex accent
        ("&#227;"    . #\ã) ; small a, tilde
        ("&#228;"    . #\ä) ; small a, umlaut mark
        ("&#229;"    . #\å) ; small a, ring
        ("&#230;"    . #\æ) ; small ae
        ("&#231;"    . #\ç) ; small c, cedilla
        ("&#232;"    . #\è) ; small e, grave accent
        ("&#233;"    . #\é) ; small e, acute accent
        ("&#234;"    . #\ê) ; small e, circumflex accent
        ("&#235;"    . #\ë) ; small e, umlaut mark
        ("&#236;"    . #\ì) ; small i, grave accent
        ("&#237;"    . #\í) ; small i, acute accent
        ("&#238;"    . #\î) ; small i, circumflex accent
        ("&#239;"    . #\ï) ; small i, umlaut mark
        ("&#240;"    . #\ð) ; small eth, Icelandic
        ("&#241;"    . #\ñ) ; small n, tilde
        ("&#242;"    . #\ò) ; small o, grave accent
        ("&#243;"    . #\ó) ; small o, acute accent
        ("&#244;"    . #\ô) ; small o, circumflex accent
        ("&#245;"    . #\õ) ; small o, tilde
        ("&#246;"    . #\ö) ; small o, umlaut mark
        ("&#248;"    . #\ø) ; small o, slash
        ("&#249;"    . #\ù) ; small u, grave accent
        ("&#250;"    . #\ú) ; small u, acute accent
        ("&#251;"    . #\û) ; small u, circumflex accent
        ("&#252;"    . #\ü) ; small u, umlaut mark
        ("&#253;"    . #\ý) ; small y, acute accent
        ("&#254;"    . #\þ) ; small thorn, Icelandic
        ("&#255;"    . #\ÿ) ; small y, umlaut mark
        ))
