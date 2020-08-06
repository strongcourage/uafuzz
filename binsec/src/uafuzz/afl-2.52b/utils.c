#include "config.h"
#include "types.h"
#include "debug.h"
#include "alloc-inl.h"
#include "hash.h"
#include "hashset.h"
#include "utils.h"

#include <limits.h>
#include <math.h>
#include <assert.h>

static const unsigned int prime_1 = 73;
static const unsigned int prime_2 = 5009;

/* Hashset implementation for C */
hashset_t hashset_create()
{
    hashset_t set = (hashset_t) calloc(1, sizeof(struct hashset_st));

    if (set == NULL) {
        return NULL;
    }
    set->nbits = 3;
    set->capacity = (size_t)(1 << set->nbits);
    set->mask = set->capacity - 1;
    set->items = (unsigned long*) calloc(set->capacity, sizeof(size_t));
    if (set->items == NULL) {
        hashset_destroy(set);
        return NULL;
    }
    set->nitems = 0;
    set->n_deleted_items = 0;
    return set;
}

size_t hashset_num_items(hashset_t set)
{
    return set->nitems;
}

void hashset_destroy(hashset_t set)
{
    if (set) {
        free(set->items);
    }
    free(set);
}

static int hashset_add_member(hashset_t set, void *item)
{
    size_t value = (size_t)item;
    size_t ii;

    if (value == 0 || value == 1) {
        return -1;
    }

    ii = set->mask & (prime_1 * value);

    while (set->items[ii] != 0 && set->items[ii] != 1) {
        if (set->items[ii] == value) {
            return 0;
        } else {
            /* search free slot */
            ii = set->mask & (ii + prime_2);
        }
    }
    set->nitems++;
    if (set->items[ii] == 1) {
        set->n_deleted_items--;
    }
    set->items[ii] = value;
    return 1;
}

static void maybe_rehash(hashset_t set)
{
    size_t *old_items;
    size_t old_capacity, ii;


    if (set->nitems + set->n_deleted_items >= (double)set->capacity * 0.85) {
        old_items = set->items;
        old_capacity = set->capacity;
        set->nbits++;
        set->capacity = (size_t)(1 << set->nbits);
        set->mask = set->capacity - 1;
        set->items = (unsigned long*) calloc(set->capacity, sizeof(size_t));
        set->nitems = 0;
        set->n_deleted_items = 0;
        assert(set->items);
        for (ii = 0; ii < old_capacity; ii++) {
            hashset_add_member(set, (void *)old_items[ii]);
        }
        free(old_items);
    }
}

int hashset_add(hashset_t set, void *item)
{
    int rv = hashset_add_member(set, item);
    maybe_rehash(set);
    return rv;
}

int hashset_remove(hashset_t set, void *item)
{
    size_t value = (size_t)item;
    size_t ii = set->mask & (prime_1 * value);

    while (set->items[ii] != 0) {
        if (set->items[ii] == value) {
            set->items[ii] = 1;
            set->nitems--;
            set->n_deleted_items++;
            return 1;
        } else {
            ii = set->mask & (ii + prime_2);
        }
    }
    return 0;
}

int hashset_is_member(hashset_t set, void *item)
{
    size_t value = (size_t)item;
    size_t ii = set->mask & (prime_1 * value);

    while (set->items[ii] != 0) {
        if (set->items[ii] == value) {
            return 1;
        } else {
            ii = set->mask & (ii + prime_2);
        }
    }
    return 0;
}

int count_targets() {
  int total_targets = 0;
  FILE *fp = fopen(getenv("TARGETS_ENV_VAR"), "r");
  if (!fp) {
    FATAL("Cannot open targets file!");
    exit (0);
  }
  char *line = NULL;
  size_t len = 0, read;
  while ((read = getline(&line, &len, fp)) != -1) {
    if (*line)
      total_targets++;
  }
  return total_targets;
}

struct edge* parse_edges() {
  struct edge* edges = ck_alloc(sizeof(struct edge) * MAX_EDGES);
  for (int i = 0; i < MAX_EDGES; ++i) {
    edges[i].src = 0;
    edges[i].dst = 0;
    edges[i].id = 0;
  }
  char *line = NULL;
  size_t len = 0, read;
  FILE *fp = fopen(getenv("CUTEDGES_ENV_VAR"), "r");
  if (!fp) {
    FATAL("Cannot open cut edges file!");
    exit (0);
  }
  int id = 0;
  while ((read = getline(&line, &len, fp)) != -1) {
    // remove trailing newline character
    size_t ln = strlen(line) - 1;
    if (*line && line[ln] == '\n')
      line[ln] = '\0';
    int cnt = 0;
    char *infos[3];
    char *ch = strtok(line, ",");
    while (ch != NULL) {
      infos[cnt] = ch;
      cnt++;
      ch = strtok(NULL, ",");
    }
    u8 type = *infos[0];
    u32 src = strtoul(*(infos + 1), NULL, 16);
    u32 dst = strtoul(*(infos + 2), NULL, 16);
    u32 src_id = ((src >> 4) ^ (src << 8)) & (MAP_SIZE - 1);
    u32 dst_id = ((dst >> 4) ^ (dst << 8)) & (MAP_SIZE - 1);
    u32 edge_id = dst_id ^ (src_id >> 1);
    // SAYF("[parse_edges] type: %c, src: 0x%x, dst: 0x%x, edge_id: %d", type, src, dst, edge_id);
    edges[id].type = type;
    edges[id].src = src;
    edges[id].dst = dst;
    edges[id].id = edge_id;
    id++;
  }
  fclose(fp);
  return edges;
}

struct fdist* parse_func_distance() {
  struct fdist* fdists = ck_alloc(sizeof(struct fdist) * MAX_FUNCS);
  char *line = NULL;
  size_t len = 0, read;
  FILE *fp = fopen(getenv("FN_DISTANCE_ENV_VAR"), "r");
  if (!fp) {
    FATAL("Cannot open function distance file!");
    exit (0);
  }
  int id = 0;
  while ((read = getline(&line, &len, fp)) != -1) {
    char *str_addr = strtok(line, ",");
    u32 addr = strtoul(str_addr, NULL, 16);
    char *str_fdist = strtok(NULL, "");
    float fdist = (float) atof(str_fdist);
    // SAYF("[parse_distance] fname: %s, fdist: %f\n", str_addr, fdist);
    fdists[id].addr = addr;
    fdists[id].dist = fdist;
    id++;
  }
  fclose(fp);
  return fdists;
}

struct func* parse_functions() {
  struct func* funcs = ck_alloc(sizeof(struct func) * MAX_FUNCS);
  for (int i = 0; i < MAX_FUNCS; ++i) {
    funcs[i].in_trace_closure = 0;
    funcs[i].id = 0;
    funcs[i].addr = 0;
    funcs[i].dist = 0.0;
  }
  char *line = NULL;
  size_t len = 0, read;
  FILE *fp = fopen(getenv("FUNCS_ENV_VAR"), "r");
  if (!fp) {
    FATAL("Cannot open functions file!");
    exit (0);
  }
  struct fdist* fdists = parse_func_distance();
  int i = 0;
  while ((read = getline(&line, &len, fp)) != -1) {
    u32 addr = strtoul(line, NULL, 16);
    // SAYF("[parse_functions] i: %d, addr: 0x%x\n", i, addr);
    funcs[i].id = i;
    funcs[i].addr = addr;
    for (int j = 0; j < MAX_FUNCS; ++j) {
      if (fdists[j].addr == addr)
        funcs[i].dist = fdists[j].dist;
    }
    i++;
  }
  fclose(fp);
  return funcs;
}

struct func* parse_trace_closure() {
  struct func* funcs = parse_functions();
  char *line = NULL;
  size_t len = 0, read;
  FILE *fp = fopen(getenv("TRACE_CLOSURE_ENV_VAR"), "r");
  if (!fp) {
    FATAL("Cannot open targets trace closure file!");
    exit (0);
  }
  while ((read = getline(&line, &len, fp)) != -1) {
    char *str_addr = strtok(line, ",");
    u32 addr = strtoul(str_addr, NULL, 16);
    char *fname = strtok(NULL, "");
    // SAYF("[parse_trace_closure] addr: 0x%x, fname: %s", addr, fname);
    for (int i = 0; i < MAX_FUNCS; ++i) {
      if (funcs[i].addr == addr)
        funcs[i].in_trace_closure = 1;
    }
  }
  fclose(fp);
  return funcs;
}

int len_union(u16 arr1[], u16 arr2[], int len1, int len2) {
  hashset_t ids = hashset_create();
  for (int i = 0; i < len1; i++)
    hashset_add(ids, (void*) arr1[i]);
  for (int i = 0; i < len2; i++)
    hashset_add(ids, (void*) arr2[i]);
  return hashset_num_items(ids);
}

u16* get_intersection(u16 arr1[], u16 arr2[], int len1, int len2) {
  u16* res = ck_alloc(sizeof(u16) * MAX_FUNCS);
  for (int i = 0; i < MAX_FUNCS; i++)
    res[i] = SHRT_MAX;
  hashset_t ids = hashset_create();
  int len = 0;
  for (int i = 0; i < len1; i++)
    hashset_add(ids, (void*) arr1[i]);
  for (int i = 0; i < len2; i++) {
    if (hashset_is_member(ids, (void*) arr2[i])) {
      res[len] = arr2[i];
      len++;
    }
  }
  return res;
}

double covered_func_similarity(struct func* funcs, u16* inter_ids, int len_union) {
  double sum = 0.0;
  for (int i = 0; i < MAX_FUNCS; i++) {
    if (inter_ids[i] != SHRT_MAX)
      sum += (double) 1.0 / (double) funcs[inter_ids[i]].dist;
      // SAYF("id: %d, fdist: %f, sum: %f\n", inter_ids[i], funcs[inter_ids[i]].dist, sum);
  }
  return (double)sum / (double)len_union;
}
