let sample = Node(1, Node(3, Node(2, Leaf, Leaf, ref Leaf), Node(6, Node(0, Leaf, Leaf, ref Leaf), Leaf, ref Leaf), ref Leaf),
Node(10, Node(7, Leaf, Leaf, ref Leaf), Node(8, Node(9, Node(13, Node(12, Leaf, Leaf, ref Leaf), Node(5, Node(11, Leaf, Leaf, ref Leaf), Leaf, ref Leaf), ref Leaf), Leaf, ref Leaf), Node(4, Leaf, Leaf, ref Leaf), ref Leaf), ref Leaf ), ref Leaf);;
let sample2 = Node(24, Node(26, Node(25, Leaf, Leaf, ref Leaf), Node(16, Node(10, Leaf, Leaf, ref Leaf), Leaf, ref Leaf), ref Leaf),
Node(20, Node(17, Leaf, Leaf, ref Leaf), Node(18, Node(19, Node(23, Node(22, Leaf, Leaf, ref Leaf),
Node(15, Node(21, Leaf, Leaf, ref Leaf), Leaf, ref Leaf), ref Leaf), Leaf, ref Leaf), Node(14, Leaf, Leaf, ref Leaf), ref Leaf), ref Leaf ), ref Leaf);;
let sample3 = Node(100,sample, sample2, ref Leaf);;
fastryguj sample3;;
show [] sample3;;