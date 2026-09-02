extern void *bmperf_alloc(void);
extern void bmperf_snap(void *);
extern void bmperf_report(void *, void *);
/* nbody, C: the Benchmarks Game 5-body kernel, 100 steps at dt = 0.01,
   printing energy * 1e9 before and after. Mirrors benches/nbody.ml. */
#include <stdio.h>
#include <math.h>
#define N 5
static float x[N], y[N], z[N], vx[N], vy[N], vz[N], m[N];
static float energy(void) {
    int i, j; float e = 0.0f;
    for (i = 0; i < N; i++) {
        e += 0.5f * m[i] * (vx[i]*vx[i] + vy[i]*vy[i] + vz[i]*vz[i]);
        for (j = i+1; j < N; j++) {
            float dx = x[i]-x[j], dy = y[i]-y[j], dz = z[i]-z[j];
            e -= m[i]*m[j] / sqrtf(dx*dx + dy*dy + dz*dz);
        }
    }
    return e;
}
static void advance(float dt) {
    int i, j;
    for (i = 0; i < N; i++)
        for (j = i+1; j < N; j++) {
            float dx = x[i]-x[j], dy = y[i]-y[j], dz = z[i]-z[j];
            float d2 = dx*dx + dy*dy + dz*dz;
            float mag = dt / (d2 * sqrtf(d2));
            vx[i] -= dx*m[j]*mag; vy[i] -= dy*m[j]*mag; vz[i] -= dz*m[j]*mag;
            vx[j] += dx*m[i]*mag; vy[j] += dy*m[i]*mag; vz[j] += dz*m[i]*mag;
        }
    for (i = 0; i < N; i++) { x[i] += dt*vx[i]; y[i] += dt*vy[i]; z[i] += dt*vz[i]; }
}
static int bench(void) {
    const float SM = 4.0f*3.141592653589793f*3.141592653589793f, DPY = 365.24f;
    int k;
    m[0] = SM;
    x[1]= 4.84143144246472090f;  y[1]=-1.16032004402742839f;  z[1]=-0.103622044471123109f;
    vx[1]= 0.00166007664274403694f*DPY; vy[1]= 0.00769901118419740425f*DPY; vz[1]=-0.0000690460016972063023f*DPY;
    m[1]= 0.000954791938424326609f*SM;
    x[2]= 8.34336671824457987f;  y[2]= 4.12479856412430479f;  z[2]=-0.403523417114321381f;
    vx[2]=-0.00276742510726862411f*DPY; vy[2]= 0.00499852801234917238f*DPY; vz[2]= 0.0000230417297573763929f*DPY;
    m[2]= 0.000285885980666130812f*SM;
    x[3]= 12.8943695621391310f;  y[3]=-15.1111514016986312f;  z[3]=-0.223307578892655734f;
    vx[3]= 0.00296460137564761618f*DPY; vy[3]= 0.00237847173959480950f*DPY; vz[3]=-0.0000296589568540237556f*DPY;
    m[3]= 0.0000436624404335156298f*SM;
    x[4]= 15.3796971148509165f;  y[4]=-25.9193146099879641f;  z[4]= 0.179258772950371181f;
    vx[4]= 0.00268067772490389322f*DPY; vy[4]= 0.00162824170038242295f*DPY; vz[4]=-0.0000951592254519715870f*DPY;
    m[4]= 0.0000515138902046611451f*SM;
    printf("%d\n", (int)(energy() * 1000000000.0f));
    for (k = 0; k < 100; k++) advance(0.01f);
    printf("%d\n", (int)(energy() * 1000000000.0f));
    return 0;
}

int main(void) {
    void *s = bmperf_alloc(), *e = bmperf_alloc();
    bmperf_snap(s);
    int r = bench();
    bmperf_snap(e);
    bmperf_report(s, e);
    return r;
}
