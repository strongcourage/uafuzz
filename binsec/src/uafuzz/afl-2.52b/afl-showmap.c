/*
   american fuzzy lop - map display utility
   ----------------------------------------

   Written and maintained by Michal Zalewski <lcamtuf@google.com>

   Copyright 2013, 2014, 2015, 2016, 2017 Google Inc. All rights reserved.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at:

     http://www.apache.org/licenses/LICENSE-2.0

   A very simple tool that runs the targeted binary and displays
   the contents of the trace bitmap in a human-readable form. Useful in
   scripts to eliminate redundant inputs and perform other checks.

   Exit code is 2 if the target program crashes; 1 if it times out or
   there is a problem executing it; or 0 if execution is successful.

 */

#define AFL_MAIN

#include "config.h"
#include "types.h"
#include "debug.h"
#include "alloc-inl.h"
#include "hash.h"

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <errno.h>
#include <signal.h>
#include <dirent.h>
#include <fcntl.h>

#include <sys/wait.h>
#include <sys/time.h>
#include <sys/shm.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/resource.h>

// UAFuzz
#include "utils.h"
#include <stdbool.h>
#include <limits.h>
#include <assert.h>
#include <math.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>

static s32 child_pid;                 /* PID of the tested program         */

static u8* trace_bits;                /* SHM with instrumentation bitmap   */

static u8 *out_file,                  /* Trace output file                 */
          *doc_path,                  /* Path to docs                      */
          *target_path,               /* Path to target binary             */
          *at_file;                   /* Substitution string for @@        */

static u32 exec_tmout;                /* Exec timeout (ms)                 */

static u64 mem_limit = MEM_LIMIT;     /* Memory limit (MB)                 */

static s32 shm_id;                    /* ID of the SHM region              */

static u8  quiet_mode,                /* Hide non-essential messages?      */
           edges_only,                /* Ignore hit counts?                */
           cmin_mode,                 /* Generate output in afl-cmin mode? */
           binary_mode,               /* Write output as a binary map      */
           keep_cores;                /* Allow coredumps?                  */

static volatile u8
           stop_soon,                 /* Ctrl-C pressed?                   */
           child_timed_out,           /* Child timed out?                  */
           child_crashed;             /* Child crashed?                    */

/* Classify tuple counts. Instead of mapping to individual bits, as in
   afl-fuzz.c, we map to more user-friendly numbers between 1 and 8. */

static const u8 count_class_human[256] = {

  [0]           = 0,
  [1]           = 1,
  [2]           = 2,
  [3]           = 3,
  [4 ... 7]     = 4,
  [8 ... 15]    = 5,
  [16 ... 31]   = 6,
  [32 ... 127]  = 7,
  [128 ... 255] = 8

};

static const u8 count_class_binary[256] = {

  [0]           = 0,
  [1]           = 1,
  [2]           = 2,
  [3]           = 4,
  [4 ... 7]     = 8,
  [8 ... 15]    = 16,
  [16 ... 31]   = 32,
  [32 ... 127]  = 64,
  [128 ... 255] = 128

};

// UAFuzz
u8 run_uafuzz, run_aflgob, run_heb;
u8 metric_dist, metric_cut, metric_func;
static char* metrics;
static struct edge* edges;
static struct func* funcs;

static void classify_counts(u8* mem, const u8* map) {

  u32 i = MAP_SIZE;

  if (edges_only) {

    while (i--) {
      if (*mem) *mem = 1;
      mem++;
    }

  } else {

    while (i--) {
      *mem = map[*mem];
      mem++;
    }

  }

}


/* Get rid of shared memory (atexit handler). */

static void remove_shm(void) {

  shmctl(shm_id, IPC_RMID, NULL);

}


/* Configure shared memory. */

static void setup_shm(void) {

  u8* shm_str;

  // UAFuzz: Increase the shared memory
  if (run_aflgob)
    shm_id = shmget(IPC_PRIVATE, MAP_SIZE + DISTANCE_SIZE, IPC_CREAT | IPC_EXCL | 0600);
  else if (run_heb)
    shm_id = shmget(IPC_PRIVATE, MAP_SIZE + DISTANCE_SIZE + TRACE_FUNCS, IPC_CREAT | IPC_EXCL | 0600);
  else if (run_uafuzz)
    shm_id = shmget(IPC_PRIVATE, MAP_SIZE + DISTANCE_SIZE + 2*TRACE_TARGETS, IPC_CREAT | IPC_EXCL | 0600);
  else
    shm_id = shmget(IPC_PRIVATE, MAP_SIZE, IPC_CREAT | IPC_EXCL | 0600);

  if (shm_id < 0) PFATAL("shmget() failed");

  atexit(remove_shm);

  shm_str = alloc_printf("%d", shm_id);

  setenv(SHM_ENV_VAR, shm_str, 1);

  ck_free(shm_str);

  trace_bits = shmat(shm_id, NULL, 0);
  
  if (!trace_bits) PFATAL("shmat() failed");

}

/* Write results. */

static u32 write_results(void) {

  s32 fd;
  u32 i, ret = 0;

  u8  cco = !!getenv("AFL_CMIN_CRASHES_ONLY"),
      caa = !!getenv("AFL_CMIN_ALLOW_ANY");

  if (!strncmp(out_file, "/dev/", 5)) {

    fd = open(out_file, O_WRONLY, 0600);
    if (fd < 0) PFATAL("Unable to open '%s'", out_file);

  } else if (!strcmp(out_file, "-")) {

    fd = dup(1);
    if (fd < 0) PFATAL("Unable to open stdout");

  } else {

    unlink(out_file); /* Ignore errors */
    fd = open(out_file, O_WRONLY | O_CREAT | O_EXCL, 0600);
    if (fd < 0) PFATAL("Unable to create '%s'", out_file);

  }


  if (binary_mode) {

    for (i = 0; i < MAP_SIZE; i++)
      if (trace_bits[i]) ret++;
    
    ck_write(fd, trace_bits, MAP_SIZE, out_file);
    close(fd);

  } else {

    FILE* f = fdopen(fd, "w");

    if (!f) PFATAL("fdopen() failed");

    for (i = 0; i < MAP_SIZE; i++) {

      if (!trace_bits[i]) continue;
      ret++;

      if (cmin_mode) {

        if (child_timed_out) break;
        if (!caa && child_crashed != cco) break;

        fprintf(f, "%u%u\n", trace_bits[i], i);

      } else fprintf(f, "%06u:%u\n", i, trace_bits[i]);

    }
  
    fclose(f);

  }

  return ret;

}


/* Handle timeout signal. */

static void handle_timeout(int sig) {

  child_timed_out = 1;
  if (child_pid > 0) kill(child_pid, SIGKILL);

}


/* Execute target application. */

static void run_target(char** argv) {

  static struct itimerval it;
  int status = 0;

  if (!quiet_mode)
    SAYF("-- Program output begins --\n" cRST);

  MEM_BARRIER();

  child_pid = fork();

  if (child_pid < 0) PFATAL("fork() failed");

  if (!child_pid) {

    struct rlimit r;

    if (quiet_mode) {

      s32 fd = open("/dev/null", O_RDWR);

      if (fd < 0 || dup2(fd, 1) < 0 || dup2(fd, 2) < 0) {
        *(u32*)trace_bits = EXEC_FAIL_SIG;
        PFATAL("Descriptor initialization failed");
      }

      close(fd);

    }

    if (mem_limit) {

      r.rlim_max = r.rlim_cur = ((rlim_t)mem_limit) << 20;

#ifdef RLIMIT_AS

      setrlimit(RLIMIT_AS, &r); /* Ignore errors */

#else

      setrlimit(RLIMIT_DATA, &r); /* Ignore errors */

#endif /* ^RLIMIT_AS */

    }

    if (!keep_cores) r.rlim_max = r.rlim_cur = 0;
    else r.rlim_max = r.rlim_cur = RLIM_INFINITY;

    setrlimit(RLIMIT_CORE, &r); /* Ignore errors */

    if (!getenv("LD_BIND_LAZY")) setenv("LD_BIND_NOW", "1", 0);

    setsid();

    execv(target_path, argv);

    *(u32*)trace_bits = EXEC_FAIL_SIG;
    exit(0);

  }

  /* Configure timeout, wait for child, cancel timeout. */

  if (exec_tmout) {

    child_timed_out = 0;
    it.it_value.tv_sec = (exec_tmout / 1000);
    it.it_value.tv_usec = (exec_tmout % 1000) * 1000;

  }

  setitimer(ITIMER_REAL, &it, NULL);

  if (waitpid(child_pid, &status, 0) <= 0) FATAL("waitpid() failed");

  child_pid = 0;
  it.it_value.tv_sec = 0;
  it.it_value.tv_usec = 0;
  setitimer(ITIMER_REAL, &it, NULL);

  MEM_BARRIER();

  /* Clean up bitmap, analyze exit condition, etc. */

  if (*(u32*)trace_bits == EXEC_FAIL_SIG)
    FATAL("Unable to execute '%s'", argv[0]);

  classify_counts(trace_bits, binary_mode ?
                  count_class_binary : count_class_human);

  if (!quiet_mode)
    SAYF(cRST "-- Program output ends --\n");

  if (!child_timed_out && !stop_soon && WIFSIGNALED(status))
    child_crashed = 1;

  if (!quiet_mode) {

    if (child_timed_out)
      SAYF(cLRD "\n+++ Program timed off +++\n" cRST);
    else if (stop_soon)
      SAYF(cLRD "\n+++ Program aborted by user +++\n" cRST);
    else if (child_crashed)
      SAYF(cLRD "\n+++ Program killed by signal %u +++\n" cRST, WTERMSIG(status));

  }


}


/* Handle Ctrl-C and the like. */

static void handle_stop_sig(int sig) {

  stop_soon = 1;

  if (child_pid > 0) kill(child_pid, SIGKILL);

}


/* Do basic preparations - persistent fds, filenames, etc. */

static void set_up_environment(void) {

  setenv("ASAN_OPTIONS", "abort_on_error=1:"
                         "detect_leaks=0:"
                         "symbolize=0:"
                         "allocator_may_return_null=1", 0);

  setenv("MSAN_OPTIONS", "exit_code=" STRINGIFY(MSAN_ERROR) ":"
                         "symbolize=0:"
                         "abort_on_error=1:"
                         "allocator_may_return_null=1:"
                         "msan_track_origins=0", 0);

  if (getenv("AFL_PRELOAD")) {
    setenv("LD_PRELOAD", getenv("AFL_PRELOAD"), 1);
    setenv("DYLD_INSERT_LIBRARIES", getenv("AFL_PRELOAD"), 1);
  }

}


/* Setup signal handlers, duh. */

static void setup_signal_handlers(void) {

  struct sigaction sa;

  sa.sa_handler   = NULL;
  sa.sa_flags     = SA_RESTART;
  sa.sa_sigaction = NULL;

  sigemptyset(&sa.sa_mask);

  /* Various ways of saying "stop". */

  sa.sa_handler = handle_stop_sig;
  sigaction(SIGHUP, &sa, NULL);
  sigaction(SIGINT, &sa, NULL);
  sigaction(SIGTERM, &sa, NULL);

  /* Exec timeout notifications. */

  sa.sa_handler = handle_timeout;
  sigaction(SIGALRM, &sa, NULL);

}


/* Detect @@ in args. */

static void detect_file_args(char** argv) {

  u32 i = 0;
  u8* cwd = getcwd(NULL, 0);

  if (!cwd) PFATAL("getcwd() failed");

  while (argv[i]) {

    u8* aa_loc = strstr(argv[i], "@@");

    if (aa_loc) {

      u8 *aa_subst, *n_arg;

      if (!at_file) FATAL("@@ syntax is not supported by this tool.");

      /* Be sure that we're always using fully-qualified paths. */

      if (at_file[0] == '/') aa_subst = at_file;
      else aa_subst = alloc_printf("%s/%s", cwd, at_file);

      /* Construct a replacement argv value. */

      *aa_loc = 0;
      n_arg = alloc_printf("%s%s%s", argv[i], aa_subst, aa_loc + 2);
      argv[i] = n_arg;
      *aa_loc = '@';

      if (at_file[0] != '/') ck_free(aa_subst);

    }

    i++;

  }

  free(cwd); /* not tracked */

}


/* Show banner. */

static void show_banner(void) {

  SAYF(cCYA "UAFuzz afl-showmap " cBRI VERSION cRST " by <lcamtuf@google.com>\n");

}

/* Display usage hints. */

static void usage(u8* argv0) {

  show_banner();

  SAYF("\n%s [ options ] -- /path/to/target_app [ ... ]\n\n"

       "Required parameters:\n\n"

       "  -o file       - file to write the trace data to\n\n"

       "Execution control settings:\n\n"

       "  -t msec       - timeout for each run (none)\n"
       "  -m megs       - memory limit for child process (%u MB)\n"
       "  -Q            - use binary-only instrumentation (QEMU mode)\n\n"

       "Other settings:\n\n"

       "  -q            - sink program's output and don't show messages\n"
       "  -e            - show edge coverage only, ignore hit counts\n"
       "  -c            - allow core dumps\n\n"

       "This tool displays raw tuple data captured by AFL instrumentation.\n"
       "For additional help, consult %s/README.\n\n" cRST,

       argv0, MEM_LIMIT, doc_path);

  exit(1);

}


/* Find binary. */

static void find_binary(u8* fname) {

  u8* env_path = 0;
  struct stat st;

  if (strchr(fname, '/') || !(env_path = getenv("PATH"))) {

    target_path = ck_strdup(fname);

    if (stat(target_path, &st) || !S_ISREG(st.st_mode) ||
        !(st.st_mode & 0111) || st.st_size < 4)
      FATAL("Program '%s' not found or not executable", fname);

  } else {

    while (env_path) {

      u8 *cur_elem, *delim = strchr(env_path, ':');

      if (delim) {

        cur_elem = ck_alloc(delim - env_path + 1);
        memcpy(cur_elem, env_path, delim - env_path);
        delim++;

      } else cur_elem = ck_strdup(env_path);

      env_path = delim;

      if (cur_elem[0])
        target_path = alloc_printf("%s/%s", cur_elem, fname);
      else
        target_path = ck_strdup(fname);

      ck_free(cur_elem);

      if (!stat(target_path, &st) && S_ISREG(st.st_mode) &&
          (st.st_mode & 0111) && st.st_size >= 4) break;

      ck_free(target_path);
      target_path = 0;

    }

    if (!target_path) FATAL("Program '%s' not found or not executable", fname);

  }

}


/* Fix up argv for QEMU. */

static char** get_qemu_argv(u8* own_loc, char** argv, int argc) {

  char** new_argv = ck_alloc(sizeof(char*) * (argc + 4));
  u8 *tmp, *cp, *rsl, *own_copy;

  /* Workaround for a QEMU stability glitch. */

  setenv("QEMU_LOG", "nochain", 1);

  memcpy(new_argv + 3, argv + 1, sizeof(char*) * argc);

  new_argv[2] = target_path;
  new_argv[1] = "--";

  /* Now we need to actually find qemu for argv[0]. */

  tmp = getenv("AFL_PATH");

  if (tmp) {

    // UAFuzz: Get corresponding afl-qemu-trace
    if (getenv("RUN_AFLGOB"))
      cp = alloc_printf("%s/afl-qemu-trace-aflgob", tmp);
    else if (getenv("RUN_HEB"))
      cp = alloc_printf("%s/afl-qemu-trace-heb", tmp);
    else if (getenv("RUN_UAFUZZ"))
      cp = alloc_printf("%s/afl-qemu-trace-uafuzz", tmp);

    if (access(cp, X_OK))
      FATAL("Unable to find '%s'", tmp);

    target_path = new_argv[0] = cp;
    return new_argv;

  }

  FATAL("Unable to find 'afl-qemu-trace'.");

}


/* Main entry point */

value uafuzz_showmap(value params, value cmd, value nb_args) {

  // UAFuzz
  CAMLparam3(params, cmd, nb_args);

  s32 opt;
  u8  mem_limit_given = 0, timeout_given = 0;
  u32 tcnt;
  char** use_argv;

  doc_path = "docs";

  char* orig_cmdline = String_val(cmd);
  int x = Int_val(nb_args);
  char* cmdline = strdup(String_val(cmd));
  SAYF("cmdline: %s\n", cmdline);
  char** args = malloc((x + 1) * sizeof(char*));
  char* token;
  int cpt_args = 0;
  while ((token = strsep(&cmdline, " "))) {
    args[cpt_args] = token;
    cpt_args++;
  }
  args[cpt_args] = NULL;

  out_file = String_val(Field(params, 2));
  u8 qemu_mode = Int_val(Field(params, 3));
  u32 timeout = Int_val(Field(params, 5));
  u8* mem_arg = String_val(Field(params, 6));
  u8* targets = String_val(Field(params, 8));
  metrics = String_val(Field(params, 9));
  if (!strcmp(metrics, "aflgob")) {
    run_aflgob = 1;
    metric_dist = 1;
    setenv("RUN_AFLGOB", "run aflgob", 1);
  }
  else if(!strcmp(metrics, "heb")) {
    run_heb = 1;
    metric_dist = 1;
    metric_func = 1;
    setenv("RUN_HEB", "run heb", 1);
  }
  else if (!strcmp(metrics, "uafuzz")) {
    run_uafuzz = 1;
    metric_dist = 1;
    metric_cut = 1;
    setenv("RUN_UAFUZZ", "run uafuzz", 1);
  }
  if (metric_dist) setenv("METRIC_DIST", "metric distance", 1);
  if (metric_cut) setenv("METRIC_CUT", "metric cut edges", 1);
  if (metric_func) setenv("METRIC_FUNC", "metric functions", 1);
  // DEBUG
  SAYF("nb_args: %d\n", x);
  SAYF("out_file: %s\n", out_file);
  SAYF("qemu_mode: %d\n", qemu_mode);
  SAYF("timeout: %d\n", timeout);
  SAYF("mem_arg: %s\n", mem_arg);
  SAYF("targets: %s\n", targets);
  SAYF("input metrics: %s\n", metrics);

  if (strlen(mem_arg) != 0) {
    u8 suffix = 'M';

    mem_limit_given = 1;

    if (!strcmp(mem_arg, "none"))
      mem_limit = 0;

    else {
      if (sscanf(mem_arg, "%llu%c", &mem_limit, &suffix) < 1 ||
          mem_arg[0] == '-')
        FATAL("Bad syntax used for -m");

      switch (suffix) {

      case 'T': mem_limit *= 1024 * 1024; break;
      case 'G': mem_limit *= 1024; break;
      case 'k': mem_limit /= 1024; break;
      case 'M': break;

      default:  FATAL("Unsupported suffix or bad syntax for -m");

      }

      if (mem_limit < 5) FATAL("Dangerously low value of -m");

      if (sizeof(rlim_t) == 4 && mem_limit > 2000)
        FATAL("Value of -m out of range on 32-bit systems");

    }
  }

  setup_shm();
  setup_signal_handlers();

  set_up_environment();

  // UAFuzz
  if (metric_dist)
    memset(trace_bits + MAP_SIZE, 0, DISTANCE_SIZE);
  if (metric_cut) {
    memset(trace_bits + MAP_SIZE + DISTANCE_SIZE, 0, 2*TRACE_TARGETS);
    edges = parse_edges();
  }
  if (metric_func) {
    char ids[MAX_FUNCS];
    for (int i = 0; i < MAX_FUNCS; ++i)
      ids[i] = CHAR_MAX;
    memcpy(trace_bits + MAP_SIZE + DISTANCE_SIZE, ids, TRACE_FUNCS);
    funcs = parse_functions();
    funcs = parse_trace_closure();
    // for (int i = 0; i < MAX_FUNCS; ++i)
    //   if (funcs[i].addr)
    //     SAYF("[parse_functions] i: %d, addr: 0x%x, dist: %f, in_trace_closure: %d\n",
    //       funcs[i].id, funcs[i].addr, funcs[i].dist, funcs[i].in_trace_closure);
  }

  find_binary(args[0]);

  if (!quiet_mode) {
    show_banner();
    ACTF("Executing '%s'...\n", target_path);
  }

  if (qemu_mode)
    use_argv = get_qemu_argv(args[0], args, cpt_args - 1);
  else
    use_argv = args;

  run_target(use_argv);

  // UAFuzz
  if (metric_dist) {
    double average_distance = 0.0;
    double total_distance = 0.0, total_count = 0.0;
    memcpy(&total_distance, trace_bits + MAP_SIZE, 8);
    memcpy(&total_count, trace_bits + MAP_SIZE + 8, 8);
    if (total_count > 0)
      average_distance = total_distance / total_count;

    OKF("total_distance: %f, total_count: %f, average_distance: %f",
      total_distance, total_count, average_distance);
  }

  // UAFuzz
  if (metric_cut) {
    int nb_cut = 0, nb_uncut = 0;
    for (int i = 0; i < MAX_EDGES; ++i) {
      if (trace_bits[edges[i].id] > 0) {
        SAYF("[%c] 0x%x -> 0x%x: %d -> hit %d times\n", edges[i].type, edges[i].src, edges[i].dst, edges[i].id, trace_bits[edges[i].id]);
        if (edges[i].type == 'C') nb_cut++;
        else nb_uncut++;
      }
    }

    int nb_reach = 0, trace_targets = 0, trace_uaf = 0, nb_uaf = 0;
    memcpy(&trace_targets, trace_bits + MAP_SIZE + DISTANCE_SIZE, TRACE_TARGETS);
    for (int i = 0; i < 32; i++)
      if ((trace_targets >> i) & 1) nb_reach++; //else break; // remove else if counting all reached targets
    memcpy(&trace_uaf, trace_bits + MAP_SIZE + DISTANCE_SIZE + TRACE_TARGETS, TRACE_TARGETS);
    for (int i = 0; i < 32; i++)
      if ((trace_uaf >> i) & 1) nb_uaf++; //else break; // remove else if counting all reached targets

    OKF("nb_cut: %d, nb_uncut: %d", nb_cut, nb_uncut);
    OKF("trace_targets: %d, nb_reach: %d", trace_targets, nb_reach);
    OKF("trace_uaf: %d, nb_uaf: %d", trace_uaf, nb_uaf);
  }

  // UAFuzz
  if (metric_func) {
    char func_ids[MAX_FUNCS];
    memcpy(func_ids, trace_bits + MAP_SIZE + DISTANCE_SIZE, MAX_FUNCS * sizeof(char));

    int nb_covered_funs = 0, nb_trace_closure = 0;
    u16 covered_func_ids[MAX_FUNCS], trace_closure_ids[MAX_FUNCS];
    for (int i = 0; i < MAX_FUNCS; i++)
      covered_func_ids[i] = SHRT_MAX;
    for (int i = 0; i < MAX_FUNCS; i++)
      trace_closure_ids[i] = SHRT_MAX;
    for (int i = 0; i < MAX_FUNCS; i++) {
      if (func_ids[i] == 1) {
        covered_func_ids[nb_covered_funs] = (u16) i;
        nb_covered_funs++;
      }
      if (funcs[i].in_trace_closure) {
        trace_closure_ids[nb_trace_closure] = i;
        nb_trace_closure++;
      }
    }
    OKF("nb_covered_funs: %d, nb_trace_closure: %d", nb_covered_funs, nb_trace_closure);
    for (int i = 0; i < nb_covered_funs; ++i)
      printf("covered function id: %d\n", covered_func_ids[i]);
    for (int i = 0; i < nb_trace_closure; ++i)
      printf("trace closure id: %d\n", trace_closure_ids[i]);

    int len = len_union(covered_func_ids, trace_closure_ids, nb_covered_funs, nb_trace_closure);
    int len_intersection = 0;
    u16* inter_ids = get_intersection(covered_func_ids, trace_closure_ids, nb_covered_funs, nb_trace_closure);
    for (int i = 0; i < MAX_FUNCS; i++) {
      if (inter_ids[i] != SHRT_MAX)
        // printf("%d ", inter_ids[i]);
        len_intersection++;
    }
    OKF("len of union: %d, len of intersection: %d", len, len_intersection);
    double cfs = covered_func_similarity(funcs, inter_ids, len);
    OKF("covered_func_similarity: %f", cfs);
  }

  tcnt = write_results();

  if (!quiet_mode) {

    if (!tcnt) FATAL("No instrumentation detected" cRST);
    OKF("Captured %u tuples in '%s'." cRST, tcnt, out_file);

  }

  CAMLreturn(child_crashed * 2 + child_timed_out); // UAFuzz

}