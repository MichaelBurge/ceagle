#lang ceagle

// Tree:         [ 1,    [2, [3], [4]], 5]
// Reverse Tree: [ 1, 5, [2, [4], [3]]]

typedef struct tree {
  int x;
  struct tree *l;
  struct tree *r;
} tree;

typedef struct reverse_tree {
  int x;
  struct reverse_tree *r;
  struct reverse_tree *l;
} reverse_tree;

reverse_tree *reverse(tree *t) {
  return (reverse_tree*)t;

}
int main() {
  tree t_l_l;
  t_l_l.x = 3;

  tree t_l_r;
  t_l_r.x = 4;

  tree t_l;
  t_l.l = &t_l_l;
  t_l.r = &t_l_r;
  t_l.x = 2;

  tree t_r;
  t_r.x = 5;

  tree root;
  root.x = 1;
  root.l = &t_l;
  root.r = &t_r;

  reverse_tree *reversed = reverse(&root);
  __builtin_set_test_result(4);
  return (*((*((*reversed).r)).l)).x; // Should be 4 since t_l_r is 4.
}
