#!r6rs

(library
 (carneades dialogue)
 
 (export)
 
 (import (rnrs)
         (carneades argument)
         (carneades rule))
 
 ; topic language Lt
 
 ; communication language Lc = {claim, why, concede, retract, argue}
  
 ; participants A = {prop, opp}
 
 ; context K subset of Lt; as known in carneades
 
 ; moves M = N x A x Ltc x N
 ; id x player x speech-act x target
 (define-record-type move
   (fields id
           player
           speech-act
           target))
 
 ; infinite dialogues Mi {d = m1, ..., mn, ... | mi in M; id(mi)=i; target(m1)=0; target(mi) < i}
 ; finite dialogues Mf
 
 ; P: D -> Pow(M)
 ; D subset of Mf, where:
 ;    d in D and P(d)=m <=> (d,m) in D
 ; legal finite dialogues
 
 (define empty-dialogue (rule empty-dialogue (if (empty-dialogue ?d)
                                                 (next-move ?d 1 prop ?x 0))))
 
 )