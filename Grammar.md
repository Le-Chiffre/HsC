Note that '<' indicates the start of a block (higher indentation),
'>' indicates the end of one, and ';' a new line on the same block.

---

*module*    ->  < *impdecls* ; *topdecls* >
            |   < *impdecls* >
            |   < *topdecls* >
            
*impdecls*  ->  *impdecl*_1 ; ... ; *impdecl*_n
*impdecl*   ->  `import` [`qualified`] *modid* [`as` modid]
