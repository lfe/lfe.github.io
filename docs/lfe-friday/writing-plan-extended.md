# LFE Friday — Extended Set Plan

*Generated: 2026-06-04*

This is the fallback runway — **103 public modules across 18 OTP applications**
outside the core set (and excluding wx). These are more specialized but many
have real teaching value, especially for LFE developers working in specific
domains.

Reach for this list when the core set arcs need variety, or when a module
here connects naturally to something just covered in the core plan.


## Tier 1: High Teaching Value (worth dedicated arcs)

### common_test (mini-arc, 4 weeks)

The standard OTP testing framework. Pairs well with the core plan's eunit
coverage (if added).

| Week | Module | Topic |
|------|--------|-------|
| 1 | ct | ct:run_test/1, pal/2, log/2 — the core test API |
| 2 | ct_suite | Writing test suites: init_per_suite, end_per_suite, groups |
| 3 | ct_hooks | Custom test hooks for setup/teardown |
| 4 | ct_property_test | Property-based testing integration |

Other ct modules (ct_ftp, ct_ssh, ct_telnet, ct_netconfc) are niche — cover
as standalone posts if there's demand.

### compiler (mini-arc, 2 weeks)

| Week | Module | Topic |
|------|--------|-------|
| 1 | compile | compile:file/1, forms/1, options | Compiling Erlang programmatically |
| 2 | cerl, cerl_trees | Core Erlang — the intermediate representation |

### eunit (2 weeks)

| Week | Module | Topic |
|------|--------|-------|
| 1 | eunit | eunit:test/1, assertions, test generators |
| 2 | eunit_surefire | XML output for CI integration |

### public_key + eldap (2 weeks)

| Week | Module | Topic |
|------|--------|-------|
| 1 | public_key | public_key:pem_decode/1, verify/4, sign/3 — certificate handling |
| 2 | eldap | eldap:open/1, search/2, add/3 — LDAP client |

### edoc (2 weeks)

| Week | Module | Topic |
|------|--------|-------|
| 1 | edoc | edoc:files/1, run/2 — generating documentation |
| 2 | edoc_extract, edoc_layout | Customizing doc extraction and rendering |

### observer (2 weeks)

| Week | Module | Topic |
|------|--------|-------|
| 1 | observer, etop | observer:start/0, etop:start/0 — GUI and CLI system monitoring |
| 2 | crashdump_viewer, ttb | Post-mortem analysis and trace recording |

### debugger (1 week)

| Week | Module | Topic |
|------|--------|-------|
| 1 | debugger, int, i | Interactive debugging: breakpoints, stepping, inspection |

### dialyzer (1 week)

| Week | Module | Topic |
|------|--------|-------|
| 1 | dialyzer | dialyzer:run/1 — programmatic type analysis |


## Tier 2: Domain-Specific (standalone posts)

These modules serve specific use cases. Each is a self-contained post.

### Networking Protocols

| Module | Application | Topic |
|--------|-------------|-------|
| ftp | ftp | FTP client |
| tftp | tftp | TFTP client/server |
| odbc | odbc | Database connectivity via ODBC |

### Telecom / Diameter / SNMP

These are very specialized. Only cover if the LFE community has telecom users.

| Module | Application | Topic |
|--------|-------------|-------|
| diameter | diameter | Diameter protocol client/server |
| snmp, snmpa, snmpm | snmp | SNMP agent and manager |
| megaco | megaco | Media gateway control |

The SNMP application alone has 34 public modules — a full arc if there's
demand, but likely too niche for the main series.

### Event Tracing

| Module | Application | Topic |
|--------|-------------|-------|
| et, et_collector, et_viewer | et | Event tracer for message sequence charts |

### Release Tools

| Module | Application | Topic |
|--------|-------------|-------|
| reltool | reltool | Release generation tool |


## Tier 3: Deep Cuts (for the completionists)

These modules are legitimate but rarely needed directly. Cover only if
running out of higher-value material.

| Module | Application | Notes |
|--------|-------------|-------|
| asn1ct | asn1 | ASN.1 compiler |
| cerl_clauses | compiler | Pattern match analysis on Core Erlang |
| ct_slave | common_test | Deprecated (use peer) |
| ct_snmp | common_test | SNMP testing support |
| ct_rpc | common_test | RPC in tests |
| ct_master | common_test | Distributed test execution |
| ct_testspec | common_test | Test specification files |
| ct_doctest | common_test | Doctest support |
| ct_cover | common_test | Coverage analysis in CT |
| diameter_app | diameter | Diameter application callback |
| diameter_codec | diameter | Diameter message codec |
| diameter_make | diameter | Diameter dictionary compiler |
| diameter_sctp | diameter | SCTP transport |
| diameter_tcp | diameter | TCP transport |
| diameter_transport | diameter | Transport behaviour |
| edoc_cli | edoc | EDoc command-line interface |
| edoc_doclet, edoc_doclet_chunks, edoc_doclet_markdown | edoc | Doc output backends |
| edoc_layout_chunks | edoc | Chunk-format layout |
| edoc_lib | edoc | EDoc utilities |
| edoc_run | edoc | EDoc script runner |
| et_selector | et | Event selection |
| megaco_* (9 modules) | megaco | Megaco protocol details |
| snmp* (30+ modules) | snmp | SNMP protocol details |
| tftp_logger | tftp | TFTP logging |
| unix_telnet | common_test | Telnet testing support |


## Summary

| Tier | Modules | Weeks of content |
|------|---------|-----------------|
| Tier 1: High teaching value | ~25 | 14 weeks in arcs |
| Tier 2: Domain-specific | ~10 | 10 standalone posts |
| Tier 3: Deep cuts | ~68 | As many as you want |
| **Total** | **103** | **24+ weeks minimum** |

Combined with the core plan's 130+ weeks, the full inventory provides
roughly **3 years** of weekly LFE Friday content.


## Recommended Integration with Core Plan

Rather than burning through the entire core plan before touching the extended
set, consider interleaving:

- After the core **OTP Behaviours arc** → extended **common_test** arc
  (natural pairing: "now that you can build OTP apps, here's how to test them")
- After the core **Developer Tools arc** → extended **edoc** and **dialyzer**
  posts (documentation and type analysis complement code introspection)
- After the core **Profiling arc** → extended **observer** arc
  (GUI tools complement the CLI profiling just covered)
- **compiler** arc works as a standalone interlude anywhere — it's
  inherently interesting to Lisp people who care about compilation
