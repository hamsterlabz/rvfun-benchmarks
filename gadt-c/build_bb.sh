#!/usr/bin/env bash
# build_bb.sh [bench...] - build gadt-c benchmarks as blackbird bare-metal ELFs.
#
# Same recipe as ../dhrystone/build.sh (shared bare runtime, buildroot rv32
# toolchain); one ELF per benchmark dir -> elf/<name>.elf. The benchmark .c is
# textually included by driver_bb.c (-DBENCH_C), which self-reports the GADT
# RESULT line + the shared full-PMC breakdown (bb_perf.h).
set -uo pipefail
HERE="$(cd "$(dirname "$0")" && pwd)"
BARE="$HERE/../bare"
SUP="$HERE/../embench/support"
TC=/home/cecil/work/hflow-tests/av/mk19/sw/linux32/buildroot/output/host/bin
GCC="${GCC:-$TC/riscv32-buildroot-linux-gnu-gcc}"
NITERS="${NITERS:-32}"
OUT="$HERE/elf"; mkdir -p "$OUT"

CF="-march=rv32imaf_zicsr -mabi=ilp32 -O2 -ffreestanding -fno-builtin -nostdlib \
    -nostartfiles -fno-common -fno-stack-protector -fno-pie -no-pie \
    -Wno-implicit -Wno-builtin-declaration-mismatch \
    -DNITERS=$NITERS -DGLOBAL_SCALE_FACTOR=$NITERS \
    -I$BARE -I$SUP -I$HERE/include"

benches=("$@")
if [ ${#benches[@]} -eq 0 ]; then
  benches=(); for d in "$HERE"/*/; do
    b=$(basename "$d")
    [ -f "$d/$b.c" ] && benches+=("$b")
  done
fi

ok=0; fail=0
for b in "${benches[@]}"; do
  if "$GCC" $CF -DBENCH_C="\"$HERE/$b/$b.c\"" -DBENCH_NAME="\"$b\"" \
       -T "$BARE/bb.ld" \
       "$BARE/crt0.S" "$HERE/driver_bb.c" \
       "$BARE/bb_io.c" "$BARE/bb_uart.c" "$BARE/bb_libc.c" \
       -o "$OUT/$b.elf" -lgcc 2>"$OUT/$b.build.log"; then
    echo "  OK    $b"; ok=$((ok+1))
  else
    echo "  BUILD-FAIL $b   (see $OUT/$b.build.log)"; fail=$((fail+1))
  fi
done
echo "built: $ok ok, $fail failed  -> $OUT"
