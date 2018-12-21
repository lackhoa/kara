(((Map i1 P1 X1)) ((Map i2 P2 X2))
 ((Map s1 X1 P1)) ((Map s2 X2 P2))
 ((Map t1 X1 P1)) ((Map t2 X2 P2))
 ((Map fa X1 X2)) ((Map fd P1 P2))

 ((= (c s1 i1) P1)) ((= (c s2 i2) P2))
 ((= (c t1 i1) P1)) ((= (c t2 i2) P2))

 ((= (c fd s1) (c s2 fa)))
 ((= (c fd s1) (c t2 fa)))
 ((= (c i2 fd) (c fa i1)))
 )
