# CC Verification Brief — LFE OTP Tutorial Audit

**For:** Claude Code, running on Duncan's machine with the real toolchain
(LFE 2.2.1, current Erlang/OTP, rebar3 + rebar3_lfe).
**Purpose:** Run-verify a set of claims about five 2015 LFE OTP blog posts that
are being audited for an in-place modernization. Everything below is a faithful
reproduction of code *as it appears in the 2015 posts* (plus a few corrected
variants for contrast). Your job is to **run each item and report what actually
happens**, so the audit can cite real evidence instead of inspection guesses.

The posts under audit:

1. *What is OTP?* (prose only — no code to run)
2. *Prelude to OTP* (`tut00`: closures vs processes)
3. *Creating LFE Servers with OTP, Part I* (`tut01-server` + `tut01-callback`)
4. *Creating LFE Servers with OTP, Part II* (unified `tut01`)
5. *Distributed LFE* (`tut02`: two-node `rpc` / `spawn_link`)

## How to use this brief

You can take the whole file at once or work item by item (V0–V8). For each item:

- Reproduce the code **exactly as given** (these are deliberately the *old*
  spellings — hyphenated names, `gen_server:start`, etc.).
- Run the steps.
- Report under the **"Report back"** heading for that item: the actual output,
  and a clear **CONFIRMED / REFUTED / NUANCED** verdict on the stated claim.
- Where something errors, paste the **real error** — the error itself is the
  evidence we want.
- Items marked **(best-effort)** can be skipped if the environment makes them
  awkward (e.g. distribution / epmd); just say so.

Please keep the original 2015 module/function names so line-by-line mapping to
the posts stays clean. A scratch rebar3 + rebar3_lfe project is fine; nothing
here should touch the real blog repo.

---

## V0 — Environment baseline

**Why:** the audit needs a reproducibility line ("verified on …").

Report back:

- `erl -noshell -eval 'io:format("OTP ~s~n",[erlang:system_info(otp_release)]), halt().'`
- LFE version (`rebar3 lfe versions`, or the `lfe` app vsn, or REPL banner)
- `rebar3 --version` and the `rebar3_lfe` plugin version
- OS / arch

---

## V1 — Do hyphenated names and `?`-suffixed names still compile?

**Claim under test:** module names like `tut01-server` and function names like
`amount?`, `lambda-state` still compile and run under LFE 2.2.1 with no
deprecation/warning. (Suspected TRUE; want confirmation + any warnings.)

Compile this module:

```lfe
(defmodule name-test
  (export (amount? 0) (do-thing 1)))

(defun amount? () 42)
(defun do-thing (x) (+ x 1))
```

Steps: compile it; from the REPL call `(name-test:amount?)` and
`(name-test:do-thing 1)`.

**Report back:** Do hyphenated module + function names and the `?` suffix
compile and run cleanly? Any warnings? Does the compiled `.beam`/atom keep the
hyphen (`'name-test'`) or get mangled?

---

## V2 — Prelude (`tut00`): closures vs processes, and the unquoted-`inc` suspicion

**Claim under test:** in the *Prelude* post's lambda transcript, the line
`(set st (funcall st inc))` uses **`inc` unquoted** (everywhere else the message
is the atom `'inc`/`'amount?`). As written, unquoted `inc` should be an *unbound
variable* and error; the post's transcript is therefore not reproducible as
printed. (Suspected: the transcript is wrong; it should be `'inc`.)

Module (exact repro from the post):

```lfe
(defmodule tut00
  (export (lambda-state 1) (process-state 2)))

(defun lambda-state (state-data)
  (lambda (msg)
    (case msg
      ('inc
        (lambda-state (+ 1 state-data)))
      ('amount?
        state-data))))

(defun process-state (caller state-data)
  (receive
    ('inc
      (process-state caller (+ 1 state-data)))
    ('amount?
        (! caller state-data)
        (process-state caller state-data))))
```

Steps (run BOTH spellings and compare):

```lfe
;; (a) exactly as printed in the post — unquoted inc:
(set st (tut00:lambda-state 0))
(set st (funcall st inc))      ; <-- does this error (inc unbound) or work?
;; (b) the presumably-intended spelling — quoted 'inc:
(set st (tut00:lambda-state 0))
(set st (funcall st 'inc))
(set st (funcall st 'inc))
(funcall st 'amount?)
```

Also exercise the process version as the post shows it:

```lfe
(set st (spawn 'tut00 'process-state `(,(self) 0)))
(! st 'inc)
(! st 'inc)
(! st 'amount?)
(c:flush)
```

**Report back:**
1. Verdict on the unquoted-`inc` line: error or not? (CONFIRMED/REFUTED that the
   post's transcript is non-reproducible as printed.)
2. The actual REPL rendering of a `#Fun<...>` value and a pid under current LFE
   (the post shows `#Fun<tut00.0.29422318>` and `<0.35.0>` — we want to know how
   much the cosmetic output has drifted).
3. Does `(c:flush)` still work, or has the flush helper moved?

---

## V3 — Part I: the two-module server, and the `tut01:` vs `tut01-server:` prose bug

**Claim under test:** Part I's narrative REPL transcript contains a naming bug:
it calls `(gen_server:call (tut01:server-name) 'bingo)` but the module is named
`tut01-server` (there is no `tut01` module in Part I). That call should fail with
an undefined-function error. (Suspected CONFIRMED.) Everything else in Part I's
transcript should reproduce when addressed as `tut01-server:`.

Two modules, exactly as listed in the post:

```lfe
;; tut01-server.lfe
(defmodule tut01-server
  (behaviour gen_server)
  (export all))

(defun server-name () (MODULE))
(defun callback-module () 'tut01-callback)
(defun initial-state () 0)
(defun genserver-opts () '())
(defun register-name () `#(local ,(server-name)))

(defun start ()
  (gen_server:start (register-name)
                    (callback-module)
                    (initial-state)
                    (genserver-opts)))

(defun inc ()
  (gen_server:cast (server-name) 'increment))

(defun dec ()
  (gen_server:cast (server-name) 'decrement))

(defun amount? ()
  (gen_server:call (server-name) 'amount))
```

```lfe
;; tut01-callback.lfe
(defmodule tut01-callback
  (export all))

(defun unknown-command () #(error "Unknown command."))

(defun init (initial-state)
  `#(ok ,initial-state))

(defun handle_cast
  (('increment state-data)
    `#(noreply ,(+ 1 state-data)))
  (('decrement state-data)
    `#(noreply ,(- state-data 1))))

(defun handle_call
  (('amount _caller state-data)
    `#(reply ,state-data ,state-data))
  ((message _caller state-data)
    `#(reply ,(unknown-command) ,state-data)))

(defun terminate (_reason _state-data)
  'ok)
```

Steps (reproduce the post's transcript):

```lfe
(tut01-server:start)
(tut01-server:inc)
(tut01-server:inc)
(tut01-server:amount?)        ; post claims 2
(tut01-server:inc)
(tut01-server:amount?)        ; post claims 3
;; the suspicious line, exactly as printed in the post:
(gen_server:call (tut01:server-name) 'bingo)   ; module tut01 does NOT exist
;; what the post presumably meant:
(gen_server:call (tut01-server:server-name) 'bingo)
```

**Report back:**
1. Does `(tut01:server-name)` error (undefined module/function)? Paste it.
   (CONFIRMED/REFUTED the prose bug.)
2. Does the corrected `tut01-server:server-name` form return the
   `#(error "Unknown command.")` reply the post shows?
3. Does `gen_server:start` with a callback module **different** from the server
   module still work as written under current OTP? Any warning?
4. Actual return of `(tut01-server:start)` — post shows `#(ok <0.35.0>)`.

---

## V4 — Part II: unified `tut01`, the `stop` path, and the **dead EXIT-clause** claim

This is the most important item. Two distinct claims.

### V4a — the unified module reproduces

Module, exactly as the Part II "Full Source Code" lists it:

```lfe
(defmodule tut01
  (behaviour gen_server)
  (export
    (start 0) (stop 0)
    (init 1) (handle_call 3) (handle_cast 2) (handle_info 2)
    (terminate 2) (code_change 3)
    (inc 0) (dec 0) (amount? 0)))

(defun server-name () (MODULE))
(defun callback-module () (MODULE))
(defun initial-state () 0)
(defun genserver-opts () '())
(defun register-name () `#(local ,(server-name)))
(defun unknown-command () #(error "Unknown command."))

(defun start ()
  (gen_server:start (register-name)
                    (callback-module)
                    (initial-state)
                    (genserver-opts)))

(defun stop ()
  (gen_server:call (server-name) 'stop))

(defun init (initial-state)
  `#(ok ,initial-state))

(defun handle_cast
  (('inc state-data)
    `#(noreply ,(+ 1 state-data)))
  (('dec state-data)
    `#(noreply ,(- state-data 1))))

(defun handle_call
  (('amount _caller state-data)
    `#(reply ,state-data ,state-data))
  (('stop _caller state-data)
    `#(stop shutdown ok state-data))
  ((message _caller state-data)
    `#(reply ,(unknown-command) ,state-data)))

(defun handle_info
  ((`#(EXIT ,_pid normal) state-data)
   `#(noreply ,state-data))
  ((`#(EXIT ,pid ,reason) state-data)
   (io:format "Process ~p exited! (Reason: ~p)~n" `(,pid ,reason))
   `#(noreply ,state-data))
  ((_msg state-data)
   `#(noreply ,state-data)))

(defun terminate (_reason _state-data)
  'ok)

(defun code_change (_old-version state _extra)
  `#(ok ,state))
```

Steps:

```lfe
(tut01:start)
(tut01:inc)
(tut01:amount?)
(tut01:dec)
(tut01:dec)
(tut01:amount?)
(tut01:stop)            ; post: ok
(tut01:amount?)         ; post: exception exit #(noproc ...)
```

**Report back:** does the full lifecycle reproduce? Paste the actual `stop`
result and the actual post-stop `amount?` error (the post shows a `noproc`
exit referencing `gen_server.erl, line 182` — we want the current line/shape).

### V4b — are the `#(EXIT …)` handle_info clauses reachable? (the real question)

The audit's pedagogical claim: **as written, the two `#(EXIT …)` clauses can
never fire from an actual exit signal**, because `start` uses `gen_server:start`
(not `start_link`) and `init` never calls `(process_flag 'trap_exit 'true)`.
They are reachable only if someone *manually sends* a `#(EXIT pid reason)` tuple
as an ordinary message. So the code *looks* like fault-tolerance but doesn't
wire any up.

Two experiments:

**(1) Manual message — does the clause match at all?**

```lfe
(tut01:start)
(! (whereis 'tut01) `#(EXIT ,(self) some-reason))   ; hand-crafted EXIT tuple
;; expect: the io:format clause fires ("Process ... exited!"), server stays up
(tut01:amount?)   ; still alive?
```

**(2) Real link/signal path — does a genuine exit signal reach handle_info?**

With the module **as written** (no trap_exit, started with `start`):

```lfe
(tut01:start)
;; link a victim process to the gen_server and kill it:
(set victim (spawn (lambda () (receive (_ 'ok)))))
;; from inside the server's process we can't easily link post-hoc from the REPL;
;; instead demonstrate the intent: show that the server is NOT trapping exits:
(erlang:process_info (whereis 'tut01) 'trap_exit)   ; expect #(trap_exit false)
```

Then a **corrected variant** to show the contrast — make a copy `tut01b` whose
`init` traps exits and whose `start` links:

```lfe
;; only the two changed functions; rest identical to tut01:
(defun start ()
  (gen_server:start_link (register-name)
                         (callback-module)
                         (initial-state)
                         (genserver-opts)))

(defun init (initial-state)
  (process_flag 'trap_exit 'true)
  `#(ok ,initial-state))
```

For `tut01b`, link a process to the server and kill it, and show the EXIT
message now flows through `handle_info` (the `io:format` fires). A clean way:

```lfe
(tut01b:start)
(erlang:process_info (whereis 'tut01b) 'trap_exit)   ; expect #(trap_exit true)
;; spawn_link a victim FROM the server process via a cast that links + dies,
;; or simpler: send the server a cast that spawn_links and the child crashes.
```

(Use whatever minimal mechanism is cleanest on your side to get the server
linked to a dying process — the point is only to demonstrate that with
`start_link` + `trap_exit` the EXIT clause fires, and without them it doesn't.)

**Report back:**
1. Does the manual-message experiment (1) fire the `io:format` clause? (Shows
   the clauses are reachable *only* by hand-crafted messages.)
2. `trap_exit` value for the as-written `tut01` (expect `false`).
3. For `tut01b` (corrected): `trap_exit` true, and does a real linked-process
   death now route through the EXIT clause? 
4. Verdict: CONFIRMED / NUANCED that the as-written EXIT handling is decorative
   (correct pattern, no signal path).

---

## V5 — Is `gen_fsm` deprecated, and is `gen_statem` the replacement?

**Claim under test:** the series intro and the *Distributed LFE* "Up Next"
promise an FSM tutorial built on `gen_fsm`; `gen_fsm` is deprecated and the
modern target is `gen_statem`. (Suspected CONFIRMED.)

Steps:

```lfe
;; trivial gen_fsm to provoke the deprecation warning at compile time:
(defmodule fsm-probe
  (behaviour gen_fsm)
  (export (init 1)))
(defun init (_args) `#(ok state-a #m()))
```

Compile it and capture any **deprecation warning**. Then confirm `gen_statem`
exists and note its presence as the replacement:

```lfe
(erlang:function_exported 'gen_statem 'start_link 4)
(code:which 'gen_fsm)
(code:which 'gen_statem)
```

**Report back:** the exact compiler warning for `gen_fsm` (if any), whether
`gen_fsm` is still loadable, and confirmation that `gen_statem` is present.
Note the OTP version (from V0) this was observed on.

---

## V6 — `rpc` vs `erpc` in the Distributed post

**Claim under test:** the *Distributed LFE* post uses `rpc:call/4`,
`rpc:multicall/2-3`, and `rpc:parallel_eval/1`. These still exist, but the
modern recommendation for call/multicall is the `erpc` module (better error
propagation). Want to confirm both still exist and whether `rpc` carries any
deprecation note.

Steps:

```lfe
(erlang:function_exported 'rpc 'call 4)
(erlang:function_exported 'rpc 'multicall 3)
(erlang:function_exported 'rpc 'parallel_eval 1)
(code:which 'erpc)
(erlang:function_exported 'erpc 'call 4)
(erlang:function_exported 'erpc 'multicall 2)
```

**Report back:** which of the above are `true`/present; any deprecation notes on
`rpc` functions; confirm `erpc` exists. (No distribution needed — these are
just module/function existence checks.)

---

## V7 — Distributed two-node smoke test (best-effort)

**Claim under test:** the *Distributed LFE* post's two-node, one-machine
transcript still reproduces (ping, names, rpc, remote `spawn_link` of the
Ackermann server), and we want the current cosmetic output (REPL banner is shown
as `LFE Shell V7.0` in the post; `net_adm:names` shape; `c:flush`).

This needs two named LFE nodes on one machine (epmd). If that's awkward in your
environment, skip and say so.

Node 1 / Node 2 (separate terminals):

```bash
rebar3 lfe repl -- -sname repl1
rebar3 lfe repl -- -sname repl2     # or however you launch a named LFE node
```

```lfe
;; on repl1 (substitute your real hostname):
(net_adm:ping 'repl2@HOST)          ; expect pong
(net_adm:names)                     ; expect #(ok (... ...))
(rpc:call 'repl2@HOST 'math 'pi '())
(rpc:multicall 'math 'pi '())
(rpc:parallel_eval '(#(math pi ()) #(math exp (1))))
```

Ackermann remote-server repro (define on repl1, run on repl2):

```lfe
(defun ackermann
  ((0 n) (+ n 1))
  ((m 0) (ackermann (- m 1) 1))
  ((m n) (ackermann (- m 1) (ackermann m (- n 1)))))

(defun ack-server ()
  (register 'acksvr (self))
  (ack-loop))

(defun ack-loop ()
  (receive
    (`#(,pid terminate)
      (! pid #(ok server-stopped)))
    (`#(,pid ,m ,n)
      (! pid (ackermann m n))
      (ack-loop))))
```

```lfe
(set rem-pid (spawn_link 'repl2@HOST #'ack-server/0))
(! #(acksvr repl2@HOST) `#(,(self) 3 3))
(c:flush)                           ; post expects "Shell got 61"
```

**Report back (best-effort):** current REPL banner string; whether ping/names/rpc
reproduce; the `net_adm:names` return shape; whether remote `spawn_link` +
ackermann works; the actual `c:flush` output. Note any setup friction (cookies,
`-name` vs `-sname`, epmd) — that friction is itself useful audit signal.

---

## V8 — Does the original tutorial repo still build? (best-effort)

**Claim under test:** the posts tell readers to
`git clone https://github.com/oubiwann/lfe-otp-tutorials` and build with classic
`rebar` + the dev build of `lfetool`. That toolchain is dead; the audit will
recommend rebar3 + rebar3_lfe. Want to know what actually breaks today.

Steps:

```bash
git clone https://github.com/oubiwann/lfe-otp-tutorials.git
cd lfe-otp-tutorials
ls            # note: Makefile / rebar.config / lfetool assumptions
# try the modern path:
rebar3 lfe compile   # or: rebar3 compile — report what happens
# and note whether the documented path (make repl, lfetool) is even runnable
```

**Report back:** repo layout (build files present), whether a modern
`rebar3 lfe` build succeeds or what it errors on, and whether the
post-documented `make repl` / `lfetool` path is still viable. (If the repo is
gone or private, say so — that's also a finding.)

---

## Summary table to return

Please end with a compact table so the audit can ingest it directly:

| Item | Claim | Verdict (CONFIRMED / REFUTED / NUANCED / SKIPPED) | Key evidence (1 line) |
|------|-------|---------------------------------------------------|------------------------|
| V1 | hyphen/`?` names compile | | |
| V2 | unquoted `inc` non-reproducible | | |
| V3 | `tut01:` vs `tut01-server:` prose bug | | |
| V4a | Part II lifecycle reproduces | | |
| V4b | EXIT clauses decorative (no signal path) | | |
| V5 | `gen_fsm` deprecated, `gen_statem` is replacement | | |
| V6 | `rpc` works but `erpc` is the modern call/multicall | | |
| V7 | distributed transcript reproduces | | |
| V8 | original repo / classic toolchain dead | | |

Plus V0 environment line (OTP / LFE / rebar3 versions) at the top.
