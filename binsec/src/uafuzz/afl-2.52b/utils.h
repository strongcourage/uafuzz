#ifndef _HAVE_UTILS_H
#define _HAVE_UTILS_H

#define PREFIX        1
#define NOT_PREFIX    0
#define UAF           1
#define NOT_UAF       0

struct edge {
  u8 type;
  u32 src;
  u32 dst;
  u32 id; // edge's id assigned by AFL
};

struct func {
  u8 in_trace_closure;
  u16 id;
  u32 addr;
  float dist;
};

struct fdist {
  u32 addr;
  float dist;
};

int count_targets();

struct edge* parse_edges();

struct func* parse_functions();

struct func* parse_trace_closure();

int len_union(u16 arr1[], u16 arr2[], int m, int n);

u16* get_intersection(u16 arr1[], u16 arr2[], int m, int n);

double covered_func_similarity(struct func* funcs, u16* inter_ids, int len_union);

#endif /* !_HAVE_UTILS_H */