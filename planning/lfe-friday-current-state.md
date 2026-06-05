# LFE Friday — Current State Report

*Audit date: 2026-06-04*

## Overview

The LFE Friday series ran from January 2015 through January 2016, publishing
roughly weekly. It adapted Steven Proctor's "Erlang Thursday" series into LFE,
with Robert Virding translating each post. The series produced **56 substantive
posts** covering **14 distinct Erlang modules** and approximately **95 distinct
functions**.

Against the full OTP core set of **190 publicly documented modules**, that's
**7.4% module coverage** — an enormous runway for revival.


## Posts by Module

### c — 5 posts (shell commands)

| Function | Post |
|----------|------|
| c:xm/1 | LFE Friday - c:xm/1 |
| c:m/1 | LFE Friday - c:m/1 |
| c:regs/0, c:nregs/0 | LFE Friday - c:regs/0 |
| c:i/0 | LFE Friday - c:i/0 |
| c:pid/3 | LFE Friday - c:pid/3 |

### calendar — 7 posts

| Function | Post |
|----------|------|
| calendar:local_time_to_universal_time/1, local_time_to_universal_time_dst/1 | LFE Friday - calendar:local_time_to_universal_time_dst/1 |
| calendar:day_of_the_week/1, day_of_the_week/3 | LFE Friday - calendar:day_of_the_week/3 |
| calendar:date_to_gregorian_days/1, date_to_gregorian_days/3 | LFE Friday - calendar:date_to_gregorian_days/3 |
| calendar:valid_date/1, valid_date/3 | LFE Friday - calendar:valid_date/3 |
| calendar:is_leap_year/1 | LFE Friday - calendar:is_leap_year/1 |
| calendar:iso_week_number/1 | LFE Friday - calendar:iso_week_number/1 |

### dict — 1 post

| Function | Post |
|----------|------|
| dict:merge/3 (also from_list/1, to_list/1) | LFE Friday - dict:merge/3 |

### digraph — 7 posts

| Function | Post |
|----------|------|
| digraph overview (new/0, new/1, add_vertex/3, vertices/1) | LFE Friday - The digraph module |
| digraph:add_vertex/1,2,3 | LFE Friday - digraph:add_vertex/1 |
| digraph:add_edge/3,4,5 | LFE Friday - digraph:add_edge/4 |
| digraph:get_path/3, get_short_path/3 | LFE Friday - digraph:get_path/3 |
| digraph:in_neighbours/2, out_neighbours/2 | LFE Friday - digraph:in_neighbors/2 |
| digraph:get_cycle/2, get_short_cycle/2 | LFE Friday - digraph:get_cycle/2 |
| digraph:del_path/3 | LFE Friday - digraph:del_path/3 |

### erlang — 3 posts (including 1 bonus)

| Function | Post |
|----------|------|
| erlang:apply/2, apply/3 | LFE Friday - erlang:apply/3 |
| erlang:list_to_atom/1 | LFE Friday - erlang:list_to_atom/1 |
| erlang:length/1 (perf analysis) | LFE Friday Bonus - Performance of erlang:length/1 |

### erl_tar — 3 posts

| Function | Post |
|----------|------|
| erl_tar:create/2, create/3 | LFE Friday - erl_tar:create/2 |
| erl_tar:extract/1, extract/2 | LFE Friday - erl_tar:extract/1 |
| erl_tar:table/1, table/2 | LFE Friday - erl_tar:table/1 |

### ets — 10 posts (5-part intro + 5 data matching)

| Topic | Post |
|-------|------|
| new/2, info/1, insert/2, lookup/2 | ETS Introduction, part 1 |
| give_away/3, heir option | ETS Introduction, part 2 |
| Table types (set, ordered_set, bag, duplicate_bag) | ETS Introduction, part 3 |
| Access protections (public, protected, private) | ETS Introduction, part 4 |
| keypos, read_concurrency, write_concurrency | ETS Introduction, part 5 |
| match/2, match_object/2 | ETS data matching |
| select/2, match_delete/2 | More ETS data matching |
| match_specs, fun2ms | ETS, match_specs and functions |
| select/3 with limit | Using ETS select with a limit |
| select continuations, select_reverse, concurrent inserts | ETS selects, continuations and inserts |

### filelib — 1 post

| Function | Post |
|----------|------|
| filelib:is_file/1 | LFE Friday - filelib:is_file/1 |

### httpc — 1 post

| Function | Post |
|----------|------|
| httpc:request/1, httpc:request/4 (+ inets:start/0) | LFE Friday - httpc:request/1 and httpc:request/4 |

### lists — 6 posts

| Function | Post |
|----------|------|
| lists:any/2, lists:all/2 | LFE Friday - lists:any/2 |
| lists:filter/2 | LFE Friday - lists:filter/2 |
| lists:dropwhile/2 | LFE Friday - lists:dropwhile/2 |
| lists:flatmap/2 | LFE Friday - lists:flatmap/2 |
| lists:delete/2 | LFE Friday - lists:delete/2 |

### ordsets — 5 posts

| Function | Post |
|----------|------|
| ordsets:is_disjoint/2 | LFE Friday - ordsets:is_disjoint/2 |
| ordsets:union/1, union/2 | LFE Friday - ordsets:union/2 |
| ordsets:intersection/1, intersection/2 | LFE Friday - ordsets:intersection/2 |
| ordsets:subtract/2 | LFE Friday - ordsets:subtract/2 |
| ordsets:is_subset/2 | LFE Friday - ordsets:is_subset/2 |

### queue — 6 posts

| Function | Post |
|----------|------|
| queue:cons/2, snoc/2 (Okasaki API) | LFE Friday - queue:cons/2 |
| queue:head/1, daeh/1, last/1, is_empty/1 | LFE Friday - queue:head/1 |
| queue:tail/1, liat/1, init/1, drop/1, drop_r/1 | LFE Friday - queue:tail/1 |
| queue:peek/1, peek_r/1 (Extended API) | LFE Friday - queue:peek/1 |
| queue:split/2, join/2 (Original API) | LFE Friday - queue:split/2 |
| queue:out/1, out_r/1 (Original API) | LFE Friday - queue:out/1 |

### string — 2 posts

| Function | Post |
|----------|------|
| string:tokens/2 | LFE Friday - string:tokens/2 |
| string:join/2 | LFE Friday - string:join/2 |

### timer — 1 post

| Function | Post |
|----------|------|
| timer:tc/1,2,3, timer:now_diff/2 | LFE Friday - timer:tc/3 |


## Coverage Patterns

**What was covered well:**

The series excels at data structure modules. ETS got a comprehensive 10-post
arc. digraph got 7 posts. queue was walked through all three API layers
(Okasaki, Extended, Original). ordsets got the full classical set-operations
treatment. These are the standout sequences.

**What was barely touched:**

Most modules got only 1-3 posts. lists has just 6 posts despite having 60+
exported functions. string got 2 posts covering functions that are now
deprecated (tokens/2, join/2 — replaced by the OTP 20 unicode-aware API).
erlang got only 3 posts for a module with 200+ BIFs.

**What was never touched:**

The entire OTP behaviour layer: gen_server, gen_statem, gen_event, supervisor.
All of I/O: io, file, filename. All of networking: gen_tcp, gen_udp, inet,
socket, ssl. All modern additions: maps, json, rand, uri_string, logger,
persistent_term, atomics, counters. All of crypto, mnesia, ssh. All profiling
tools: cover, cprof, eprof, fprof, tprof, dbg.

**Chronological arc:**

Jan–Mar 2015: functional list ops, string, dict, timer, httpc, erlang basics.
Apr–May 2015: calendar deep-dive, filelib.
May–Jul 2015: queue (6 posts), ordsets (5 posts), erlang bonus.
Aug–Sep 2015: c module (5 posts), erl_tar (3 posts).
Oct–Nov 2015: digraph (7 posts).
Nov 2015–Jan 2016: ETS (10 posts).

The series ended mid-run after the ETS arc. There was no wind-down; it simply
stopped publishing.


## The Opportunity

With 176 uncovered core modules and 103 extended modules, there is material
for **years** of weekly LFE Friday posts. The most impactful gaps are the OTP
behaviours (gen_server, gen_statem, supervisor), the maps module, the modern
I/O and networking stack, and the profiling/debugging tools — these are what
working Erlang/LFE developers use daily.

See `lfe-friday-core-plan.md` for the prioritized core roadmap and
`lfe-friday-extended-plan.md` for the extended set.
