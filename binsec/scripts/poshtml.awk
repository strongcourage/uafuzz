#
# -- poshtml.awk
#
# This script aims at circumventing some ocamldoc weirdness and
# inserting some new tags in the generated html to faciliting CSS styling
# 
# More specifically we do the following:
#
# - On record types, ocamldoc inserts a dangling closing bracket '}' without any tag.
#   We add a pre tag and enclose the record type into a dedicated <div>.
#
# - On type declarations, the first line 'type t =' is in a <pre> tag whereas
#   the constructors are enclose in a table. We add an enclosing <div> to
#   highlight the fact that they belong together
# 

BEGIN { 
# variant is either 0 (out) or 1 (in)
     variant = 0; 
# record is 0 (out),
# 1 (in variant table), 
# or 2 (out of table, waiting for closing '}' ) 
     record  = 0; 

     }

/{<\/code><\/pre><table class="typetable">/ { 
     record = 1; 
     print "<div class=\"record\">", $0;
     next;
}

/<\/code><\/pre><table class="typetable">/ { 
     if (record == 0) {
          variant = 1;
          print "<div class=\"typedecl\">", $0;
          next;
     }
}

/<br>/ { text = gsub("<br>","", $0); print "<p class=\"has-text-weight-semibold\">",$0,"</p>"; next; }

/<\/table>/ { 
     print $0;
     if (record == 1) record = 2; 
     if (variant == 1) { print "</div>"; variant = 0; }
     next;
}

/}/ { if (record == 2) { 
          record = 0;
          print "<pre class=\"closing_bracket\"><code>",$0,"</code></pre>";
          print "</div>"; 
          next; }}   

        { print $0; }

END { }
