#include <stdint.h>
#include <string.h>

#if defined(__aarch64__) && defined(__ARM_FEATURE_SHA512)

#include <arm_neon.h>

static const uint64_t K[80] = {
    0x428a2f98d728ae22ULL, 0x7137449123ef65cdULL,
    0xb5c0fbcfec4d3b2fULL, 0xe9b5dba58189dbbcULL,
    0x3956c25bf348b538ULL, 0x59f111f1b605d019ULL,
    0x923f82a4af194f9bULL, 0xab1c5ed5da6d8118ULL,
    0xd807aa98a3030242ULL, 0x12835b0145706fbeULL,
    0x243185be4ee4b28cULL, 0x550c7dc3d5ffb4e2ULL,
    0x72be5d74f27b896fULL, 0x80deb1fe3b1696b1ULL,
    0x9bdc06a725c71235ULL, 0xc19bf174cf692694ULL,
    0xe49b69c19ef14ad2ULL, 0xefbe4786384f25e3ULL,
    0x0fc19dc68b8cd5b5ULL, 0x240ca1cc77ac9c65ULL,
    0x2de92c6f592b0275ULL, 0x4a7484aa6ea6e483ULL,
    0x5cb0a9dcbd41fbd4ULL, 0x76f988da831153b5ULL,
    0x983e5152ee66dfabULL, 0xa831c66d2db43210ULL,
    0xb00327c898fb213fULL, 0xbf597fc7beef0ee4ULL,
    0xc6e00bf33da88fc2ULL, 0xd5a79147930aa725ULL,
    0x06ca6351e003826fULL, 0x142929670a0e6e70ULL,
    0x27b70a8546d22ffcULL, 0x2e1b21385c26c926ULL,
    0x4d2c6dfc5ac42aedULL, 0x53380d139d95b3dfULL,
    0x650a73548baf63deULL, 0x766a0abb3c77b2a8ULL,
    0x81c2c92e47edaee6ULL, 0x92722c851482353bULL,
    0xa2bfe8a14cf10364ULL, 0xa81a664bbc423001ULL,
    0xc24b8b70d0f89791ULL, 0xc76c51a30654be30ULL,
    0xd192e819d6ef5218ULL, 0xd69906245565a910ULL,
    0xf40e35855771202aULL, 0x106aa07032bbd1b8ULL,
    0x19a4c116b8d2d0c8ULL, 0x1e376c085141ab53ULL,
    0x2748774cdf8eeb99ULL, 0x34b0bcb5e19b48a8ULL,
    0x391c0cb3c5c95a63ULL, 0x4ed8aa4ae3418acbULL,
    0x5b9cca4f7763e373ULL, 0x682e6ff3d6b2b8a3ULL,
    0x748f82ee5defb2fcULL, 0x78a5636f43172f60ULL,
    0x84c87814a1f0ab72ULL, 0x8cc702081a6439ecULL,
    0x90befffa23631e28ULL, 0xa4506cebde82bde9ULL,
    0xbef9a3f7b2c67915ULL, 0xc67178f2e372532bULL,
    0xca273eceea26619cULL, 0xd186b8c721c0c207ULL,
    0xeada7dd6cde0eb1eULL, 0xf57d4f7fee6ed178ULL,
    0x06f067aa72176fbaULL, 0x0a637dc5a2c898a6ULL,
    0x113f9804bef90daeULL, 0x1b710b35131c471bULL,
    0x28db77f523047d84ULL, 0x32caab7b40c72493ULL,
    0x3c9ebe0a15c9bebcULL, 0x431d67c49c100d4cULL,
    0x4cc5d4becb3e42b6ULL, 0x597f299cfc657e2aULL,
    0x5fcb6fab3ad6faecULL, 0x6c44198c4a475817ULL
};

/*
 * Process one 128-byte block using ARM SHA512 crypto instructions.
 *
 * state: pointer to 8 uint64_t words (a,b,c,d,e,f,g,h)
 * block: pointer to 16 uint64_t words (already native endian)
 *
 * The state is updated in place.
 */
void sha512_block_arm(uint64_t *state, const uint64_t *block) {
    /* Load current hash state */
    uint64x2_t ab = vld1q_u64(&state[0]);
    uint64x2_t cd = vld1q_u64(&state[2]);
    uint64x2_t ef = vld1q_u64(&state[4]);
    uint64x2_t gh = vld1q_u64(&state[6]);

    /* Save original for final addition */
    uint64x2_t ab_orig = ab;
    uint64x2_t cd_orig = cd;
    uint64x2_t ef_orig = ef;
    uint64x2_t gh_orig = gh;

    /* Load message (already native endian) */
    uint64x2_t m0 = vld1q_u64(&block[0]);
    uint64x2_t m1 = vld1q_u64(&block[2]);
    uint64x2_t m2 = vld1q_u64(&block[4]);
    uint64x2_t m3 = vld1q_u64(&block[6]);
    uint64x2_t m4 = vld1q_u64(&block[8]);
    uint64x2_t m5 = vld1q_u64(&block[10]);
    uint64x2_t m6 = vld1q_u64(&block[12]);
    uint64x2_t m7 = vld1q_u64(&block[14]);

    uint64x2_t tmp;

    /* Rounds 0-1 */
    tmp = vaddq_u64(m0, vld1q_u64(&K[0]));
    tmp = vextq_u64(tmp, tmp, 1);
    tmp = vaddq_u64(gh, tmp);
    tmp = vsha512hq_u64(tmp, vextq_u64(ef, gh, 1), vextq_u64(cd, ef, 1));
    gh = vsha512h2q_u64(tmp, cd, ab);
    cd = vaddq_u64(cd, tmp);
    m0 = vsha512su1q_u64(vsha512su0q_u64(m0, m1), m7, vextq_u64(m4, m5, 1));

    /* Rounds 2-3 */
    tmp = vaddq_u64(m1, vld1q_u64(&K[2]));
    tmp = vextq_u64(tmp, tmp, 1);
    tmp = vaddq_u64(ef, tmp);
    tmp = vsha512hq_u64(tmp, vextq_u64(cd, ef, 1), vextq_u64(ab, cd, 1));
    ef = vsha512h2q_u64(tmp, ab, gh);
    ab = vaddq_u64(ab, tmp);
    m1 = vsha512su1q_u64(vsha512su0q_u64(m1, m2), m0, vextq_u64(m5, m6, 1));

    /* Rounds 4-5 */
    tmp = vaddq_u64(m2, vld1q_u64(&K[4]));
    tmp = vextq_u64(tmp, tmp, 1);
    tmp = vaddq_u64(cd, tmp);
    tmp = vsha512hq_u64(tmp, vextq_u64(ab, cd, 1), vextq_u64(gh, ab, 1));
    cd = vsha512h2q_u64(tmp, gh, ef);
    gh = vaddq_u64(gh, tmp);
    m2 = vsha512su1q_u64(vsha512su0q_u64(m2, m3), m1, vextq_u64(m6, m7, 1));

    /* Rounds 6-7 */
    tmp = vaddq_u64(m3, vld1q_u64(&K[6]));
    tmp = vextq_u64(tmp, tmp, 1);
    tmp = vaddq_u64(ab, tmp);
    tmp = vsha512hq_u64(tmp, vextq_u64(gh, ab, 1), vextq_u64(ef, gh, 1));
    ab = vsha512h2q_u64(tmp, ef, cd);
    ef = vaddq_u64(ef, tmp);
    m3 = vsha512su1q_u64(vsha512su0q_u64(m3, m4), m2, vextq_u64(m7, m0, 1));

    /* Rounds 8-9 */
    tmp = vaddq_u64(m4, vld1q_u64(&K[8]));
    tmp = vextq_u64(tmp, tmp, 1);
    tmp = vaddq_u64(gh, tmp);
    tmp = vsha512hq_u64(tmp, vextq_u64(ef, gh, 1), vextq_u64(cd, ef, 1));
    gh = vsha512h2q_u64(tmp, cd, ab);
    cd = vaddq_u64(cd, tmp);
    m4 = vsha512su1q_u64(vsha512su0q_u64(m4, m5), m3, vextq_u64(m0, m1, 1));

    /* Rounds 10-11 */
    tmp = vaddq_u64(m5, vld1q_u64(&K[10]));
    tmp = vextq_u64(tmp, tmp, 1);
    tmp = vaddq_u64(ef, tmp);
    tmp = vsha512hq_u64(tmp, vextq_u64(cd, ef, 1), vextq_u64(ab, cd, 1));
    ef = vsha512h2q_u64(tmp, ab, gh);
    ab = vaddq_u64(ab, tmp);
    m5 = vsha512su1q_u64(vsha512su0q_u64(m5, m6), m4, vextq_u64(m1, m2, 1));

    /* Rounds 12-13 */
    tmp = vaddq_u64(m6, vld1q_u64(&K[12]));
    tmp = vextq_u64(tmp, tmp, 1);
    tmp = vaddq_u64(cd, tmp);
    tmp = vsha512hq_u64(tmp, vextq_u64(ab, cd, 1), vextq_u64(gh, ab, 1));
    cd = vsha512h2q_u64(tmp, gh, ef);
    gh = vaddq_u64(gh, tmp);
    m6 = vsha512su1q_u64(vsha512su0q_u64(m6, m7), m5, vextq_u64(m2, m3, 1));

    /* Rounds 14-15 */
    tmp = vaddq_u64(m7, vld1q_u64(&K[14]));
    tmp = vextq_u64(tmp, tmp, 1);
    tmp = vaddq_u64(ab, tmp);
    tmp = vsha512hq_u64(tmp, vextq_u64(gh, ab, 1), vextq_u64(ef, gh, 1));
    ab = vsha512h2q_u64(tmp, ef, cd);
    ef = vaddq_u64(ef, tmp);
    m7 = vsha512su1q_u64(vsha512su0q_u64(m7, m0), m6, vextq_u64(m3, m4, 1));

    /* Rounds 16-17 */
    tmp = vaddq_u64(m0, vld1q_u64(&K[16]));
    tmp = vextq_u64(tmp, tmp, 1);
    tmp = vaddq_u64(gh, tmp);
    tmp = vsha512hq_u64(tmp, vextq_u64(ef, gh, 1), vextq_u64(cd, ef, 1));
    gh = vsha512h2q_u64(tmp, cd, ab);
    cd = vaddq_u64(cd, tmp);
    m0 = vsha512su1q_u64(vsha512su0q_u64(m0, m1), m7, vextq_u64(m4, m5, 1));

    /* Rounds 18-19 */
    tmp = vaddq_u64(m1, vld1q_u64(&K[18]));
    tmp = vextq_u64(tmp, tmp, 1);
    tmp = vaddq_u64(ef, tmp);
    tmp = vsha512hq_u64(tmp, vextq_u64(cd, ef, 1), vextq_u64(ab, cd, 1));
    ef = vsha512h2q_u64(tmp, ab, gh);
    ab = vaddq_u64(ab, tmp);
    m1 = vsha512su1q_u64(vsha512su0q_u64(m1, m2), m0, vextq_u64(m5, m6, 1));

    /* Rounds 20-21 */
    tmp = vaddq_u64(m2, vld1q_u64(&K[20]));
    tmp = vextq_u64(tmp, tmp, 1);
    tmp = vaddq_u64(cd, tmp);
    tmp = vsha512hq_u64(tmp, vextq_u64(ab, cd, 1), vextq_u64(gh, ab, 1));
    cd = vsha512h2q_u64(tmp, gh, ef);
    gh = vaddq_u64(gh, tmp);
    m2 = vsha512su1q_u64(vsha512su0q_u64(m2, m3), m1, vextq_u64(m6, m7, 1));

    /* Rounds 22-23 */
    tmp = vaddq_u64(m3, vld1q_u64(&K[22]));
    tmp = vextq_u64(tmp, tmp, 1);
    tmp = vaddq_u64(ab, tmp);
    tmp = vsha512hq_u64(tmp, vextq_u64(gh, ab, 1), vextq_u64(ef, gh, 1));
    ab = vsha512h2q_u64(tmp, ef, cd);
    ef = vaddq_u64(ef, tmp);
    m3 = vsha512su1q_u64(vsha512su0q_u64(m3, m4), m2, vextq_u64(m7, m0, 1));

    /* Rounds 24-25 */
    tmp = vaddq_u64(m4, vld1q_u64(&K[24]));
    tmp = vextq_u64(tmp, tmp, 1);
    tmp = vaddq_u64(gh, tmp);
    tmp = vsha512hq_u64(tmp, vextq_u64(ef, gh, 1), vextq_u64(cd, ef, 1));
    gh = vsha512h2q_u64(tmp, cd, ab);
    cd = vaddq_u64(cd, tmp);
    m4 = vsha512su1q_u64(vsha512su0q_u64(m4, m5), m3, vextq_u64(m0, m1, 1));

    /* Rounds 26-27 */
    tmp = vaddq_u64(m5, vld1q_u64(&K[26]));
    tmp = vextq_u64(tmp, tmp, 1);
    tmp = vaddq_u64(ef, tmp);
    tmp = vsha512hq_u64(tmp, vextq_u64(cd, ef, 1), vextq_u64(ab, cd, 1));
    ef = vsha512h2q_u64(tmp, ab, gh);
    ab = vaddq_u64(ab, tmp);
    m5 = vsha512su1q_u64(vsha512su0q_u64(m5, m6), m4, vextq_u64(m1, m2, 1));

    /* Rounds 28-29 */
    tmp = vaddq_u64(m6, vld1q_u64(&K[28]));
    tmp = vextq_u64(tmp, tmp, 1);
    tmp = vaddq_u64(cd, tmp);
    tmp = vsha512hq_u64(tmp, vextq_u64(ab, cd, 1), vextq_u64(gh, ab, 1));
    cd = vsha512h2q_u64(tmp, gh, ef);
    gh = vaddq_u64(gh, tmp);
    m6 = vsha512su1q_u64(vsha512su0q_u64(m6, m7), m5, vextq_u64(m2, m3, 1));

    /* Rounds 30-31 */
    tmp = vaddq_u64(m7, vld1q_u64(&K[30]));
    tmp = vextq_u64(tmp, tmp, 1);
    tmp = vaddq_u64(ab, tmp);
    tmp = vsha512hq_u64(tmp, vextq_u64(gh, ab, 1), vextq_u64(ef, gh, 1));
    ab = vsha512h2q_u64(tmp, ef, cd);
    ef = vaddq_u64(ef, tmp);
    m7 = vsha512su1q_u64(vsha512su0q_u64(m7, m0), m6, vextq_u64(m3, m4, 1));

    /* Rounds 32-33 */
    tmp = vaddq_u64(m0, vld1q_u64(&K[32]));
    tmp = vextq_u64(tmp, tmp, 1);
    tmp = vaddq_u64(gh, tmp);
    tmp = vsha512hq_u64(tmp, vextq_u64(ef, gh, 1), vextq_u64(cd, ef, 1));
    gh = vsha512h2q_u64(tmp, cd, ab);
    cd = vaddq_u64(cd, tmp);
    m0 = vsha512su1q_u64(vsha512su0q_u64(m0, m1), m7, vextq_u64(m4, m5, 1));

    /* Rounds 34-35 */
    tmp = vaddq_u64(m1, vld1q_u64(&K[34]));
    tmp = vextq_u64(tmp, tmp, 1);
    tmp = vaddq_u64(ef, tmp);
    tmp = vsha512hq_u64(tmp, vextq_u64(cd, ef, 1), vextq_u64(ab, cd, 1));
    ef = vsha512h2q_u64(tmp, ab, gh);
    ab = vaddq_u64(ab, tmp);
    m1 = vsha512su1q_u64(vsha512su0q_u64(m1, m2), m0, vextq_u64(m5, m6, 1));

    /* Rounds 36-37 */
    tmp = vaddq_u64(m2, vld1q_u64(&K[36]));
    tmp = vextq_u64(tmp, tmp, 1);
    tmp = vaddq_u64(cd, tmp);
    tmp = vsha512hq_u64(tmp, vextq_u64(ab, cd, 1), vextq_u64(gh, ab, 1));
    cd = vsha512h2q_u64(tmp, gh, ef);
    gh = vaddq_u64(gh, tmp);
    m2 = vsha512su1q_u64(vsha512su0q_u64(m2, m3), m1, vextq_u64(m6, m7, 1));

    /* Rounds 38-39 */
    tmp = vaddq_u64(m3, vld1q_u64(&K[38]));
    tmp = vextq_u64(tmp, tmp, 1);
    tmp = vaddq_u64(ab, tmp);
    tmp = vsha512hq_u64(tmp, vextq_u64(gh, ab, 1), vextq_u64(ef, gh, 1));
    ab = vsha512h2q_u64(tmp, ef, cd);
    ef = vaddq_u64(ef, tmp);
    m3 = vsha512su1q_u64(vsha512su0q_u64(m3, m4), m2, vextq_u64(m7, m0, 1));

    /* Rounds 40-41 */
    tmp = vaddq_u64(m4, vld1q_u64(&K[40]));
    tmp = vextq_u64(tmp, tmp, 1);
    tmp = vaddq_u64(gh, tmp);
    tmp = vsha512hq_u64(tmp, vextq_u64(ef, gh, 1), vextq_u64(cd, ef, 1));
    gh = vsha512h2q_u64(tmp, cd, ab);
    cd = vaddq_u64(cd, tmp);
    m4 = vsha512su1q_u64(vsha512su0q_u64(m4, m5), m3, vextq_u64(m0, m1, 1));

    /* Rounds 42-43 */
    tmp = vaddq_u64(m5, vld1q_u64(&K[42]));
    tmp = vextq_u64(tmp, tmp, 1);
    tmp = vaddq_u64(ef, tmp);
    tmp = vsha512hq_u64(tmp, vextq_u64(cd, ef, 1), vextq_u64(ab, cd, 1));
    ef = vsha512h2q_u64(tmp, ab, gh);
    ab = vaddq_u64(ab, tmp);
    m5 = vsha512su1q_u64(vsha512su0q_u64(m5, m6), m4, vextq_u64(m1, m2, 1));

    /* Rounds 44-45 */
    tmp = vaddq_u64(m6, vld1q_u64(&K[44]));
    tmp = vextq_u64(tmp, tmp, 1);
    tmp = vaddq_u64(cd, tmp);
    tmp = vsha512hq_u64(tmp, vextq_u64(ab, cd, 1), vextq_u64(gh, ab, 1));
    cd = vsha512h2q_u64(tmp, gh, ef);
    gh = vaddq_u64(gh, tmp);
    m6 = vsha512su1q_u64(vsha512su0q_u64(m6, m7), m5, vextq_u64(m2, m3, 1));

    /* Rounds 46-47 */
    tmp = vaddq_u64(m7, vld1q_u64(&K[46]));
    tmp = vextq_u64(tmp, tmp, 1);
    tmp = vaddq_u64(ab, tmp);
    tmp = vsha512hq_u64(tmp, vextq_u64(gh, ab, 1), vextq_u64(ef, gh, 1));
    ab = vsha512h2q_u64(tmp, ef, cd);
    ef = vaddq_u64(ef, tmp);
    m7 = vsha512su1q_u64(vsha512su0q_u64(m7, m0), m6, vextq_u64(m3, m4, 1));

    /* Rounds 48-49 */
    tmp = vaddq_u64(m0, vld1q_u64(&K[48]));
    tmp = vextq_u64(tmp, tmp, 1);
    tmp = vaddq_u64(gh, tmp);
    tmp = vsha512hq_u64(tmp, vextq_u64(ef, gh, 1), vextq_u64(cd, ef, 1));
    gh = vsha512h2q_u64(tmp, cd, ab);
    cd = vaddq_u64(cd, tmp);
    m0 = vsha512su1q_u64(vsha512su0q_u64(m0, m1), m7, vextq_u64(m4, m5, 1));

    /* Rounds 50-51 */
    tmp = vaddq_u64(m1, vld1q_u64(&K[50]));
    tmp = vextq_u64(tmp, tmp, 1);
    tmp = vaddq_u64(ef, tmp);
    tmp = vsha512hq_u64(tmp, vextq_u64(cd, ef, 1), vextq_u64(ab, cd, 1));
    ef = vsha512h2q_u64(tmp, ab, gh);
    ab = vaddq_u64(ab, tmp);
    m1 = vsha512su1q_u64(vsha512su0q_u64(m1, m2), m0, vextq_u64(m5, m6, 1));

    /* Rounds 52-53 */
    tmp = vaddq_u64(m2, vld1q_u64(&K[52]));
    tmp = vextq_u64(tmp, tmp, 1);
    tmp = vaddq_u64(cd, tmp);
    tmp = vsha512hq_u64(tmp, vextq_u64(ab, cd, 1), vextq_u64(gh, ab, 1));
    cd = vsha512h2q_u64(tmp, gh, ef);
    gh = vaddq_u64(gh, tmp);
    m2 = vsha512su1q_u64(vsha512su0q_u64(m2, m3), m1, vextq_u64(m6, m7, 1));

    /* Rounds 54-55 */
    tmp = vaddq_u64(m3, vld1q_u64(&K[54]));
    tmp = vextq_u64(tmp, tmp, 1);
    tmp = vaddq_u64(ab, tmp);
    tmp = vsha512hq_u64(tmp, vextq_u64(gh, ab, 1), vextq_u64(ef, gh, 1));
    ab = vsha512h2q_u64(tmp, ef, cd);
    ef = vaddq_u64(ef, tmp);
    m3 = vsha512su1q_u64(vsha512su0q_u64(m3, m4), m2, vextq_u64(m7, m0, 1));

    /* Rounds 56-57 */
    tmp = vaddq_u64(m4, vld1q_u64(&K[56]));
    tmp = vextq_u64(tmp, tmp, 1);
    tmp = vaddq_u64(gh, tmp);
    tmp = vsha512hq_u64(tmp, vextq_u64(ef, gh, 1), vextq_u64(cd, ef, 1));
    gh = vsha512h2q_u64(tmp, cd, ab);
    cd = vaddq_u64(cd, tmp);
    m4 = vsha512su1q_u64(vsha512su0q_u64(m4, m5), m3, vextq_u64(m0, m1, 1));

    /* Rounds 58-59 */
    tmp = vaddq_u64(m5, vld1q_u64(&K[58]));
    tmp = vextq_u64(tmp, tmp, 1);
    tmp = vaddq_u64(ef, tmp);
    tmp = vsha512hq_u64(tmp, vextq_u64(cd, ef, 1), vextq_u64(ab, cd, 1));
    ef = vsha512h2q_u64(tmp, ab, gh);
    ab = vaddq_u64(ab, tmp);
    m5 = vsha512su1q_u64(vsha512su0q_u64(m5, m6), m4, vextq_u64(m1, m2, 1));

    /* Rounds 60-61 */
    tmp = vaddq_u64(m6, vld1q_u64(&K[60]));
    tmp = vextq_u64(tmp, tmp, 1);
    tmp = vaddq_u64(cd, tmp);
    tmp = vsha512hq_u64(tmp, vextq_u64(ab, cd, 1), vextq_u64(gh, ab, 1));
    cd = vsha512h2q_u64(tmp, gh, ef);
    gh = vaddq_u64(gh, tmp);
    m6 = vsha512su1q_u64(vsha512su0q_u64(m6, m7), m5, vextq_u64(m2, m3, 1));

    /* Rounds 62-63 */
    tmp = vaddq_u64(m7, vld1q_u64(&K[62]));
    tmp = vextq_u64(tmp, tmp, 1);
    tmp = vaddq_u64(ab, tmp);
    tmp = vsha512hq_u64(tmp, vextq_u64(gh, ab, 1), vextq_u64(ef, gh, 1));
    ab = vsha512h2q_u64(tmp, ef, cd);
    ef = vaddq_u64(ef, tmp);
    m7 = vsha512su1q_u64(vsha512su0q_u64(m7, m0), m6, vextq_u64(m3, m4, 1));

    /* Rounds 64-65 */
    tmp = vaddq_u64(m0, vld1q_u64(&K[64]));
    tmp = vextq_u64(tmp, tmp, 1);
    tmp = vaddq_u64(gh, tmp);
    tmp = vsha512hq_u64(tmp, vextq_u64(ef, gh, 1), vextq_u64(cd, ef, 1));
    gh = vsha512h2q_u64(tmp, cd, ab);
    cd = vaddq_u64(cd, tmp);

    /* Rounds 66-67 */
    tmp = vaddq_u64(m1, vld1q_u64(&K[66]));
    tmp = vextq_u64(tmp, tmp, 1);
    tmp = vaddq_u64(ef, tmp);
    tmp = vsha512hq_u64(tmp, vextq_u64(cd, ef, 1), vextq_u64(ab, cd, 1));
    ef = vsha512h2q_u64(tmp, ab, gh);
    ab = vaddq_u64(ab, tmp);

    /* Rounds 68-69 */
    tmp = vaddq_u64(m2, vld1q_u64(&K[68]));
    tmp = vextq_u64(tmp, tmp, 1);
    tmp = vaddq_u64(cd, tmp);
    tmp = vsha512hq_u64(tmp, vextq_u64(ab, cd, 1), vextq_u64(gh, ab, 1));
    cd = vsha512h2q_u64(tmp, gh, ef);
    gh = vaddq_u64(gh, tmp);

    /* Rounds 70-71 */
    tmp = vaddq_u64(m3, vld1q_u64(&K[70]));
    tmp = vextq_u64(tmp, tmp, 1);
    tmp = vaddq_u64(ab, tmp);
    tmp = vsha512hq_u64(tmp, vextq_u64(gh, ab, 1), vextq_u64(ef, gh, 1));
    ab = vsha512h2q_u64(tmp, ef, cd);
    ef = vaddq_u64(ef, tmp);

    /* Rounds 72-73 */
    tmp = vaddq_u64(m4, vld1q_u64(&K[72]));
    tmp = vextq_u64(tmp, tmp, 1);
    tmp = vaddq_u64(gh, tmp);
    tmp = vsha512hq_u64(tmp, vextq_u64(ef, gh, 1), vextq_u64(cd, ef, 1));
    gh = vsha512h2q_u64(tmp, cd, ab);
    cd = vaddq_u64(cd, tmp);

    /* Rounds 74-75 */
    tmp = vaddq_u64(m5, vld1q_u64(&K[74]));
    tmp = vextq_u64(tmp, tmp, 1);
    tmp = vaddq_u64(ef, tmp);
    tmp = vsha512hq_u64(tmp, vextq_u64(cd, ef, 1), vextq_u64(ab, cd, 1));
    ef = vsha512h2q_u64(tmp, ab, gh);
    ab = vaddq_u64(ab, tmp);

    /* Rounds 76-77 */
    tmp = vaddq_u64(m6, vld1q_u64(&K[76]));
    tmp = vextq_u64(tmp, tmp, 1);
    tmp = vaddq_u64(cd, tmp);
    tmp = vsha512hq_u64(tmp, vextq_u64(ab, cd, 1), vextq_u64(gh, ab, 1));
    cd = vsha512h2q_u64(tmp, gh, ef);
    gh = vaddq_u64(gh, tmp);

    /* Rounds 78-79 */
    tmp = vaddq_u64(m7, vld1q_u64(&K[78]));
    tmp = vextq_u64(tmp, tmp, 1);
    tmp = vaddq_u64(ab, tmp);
    tmp = vsha512hq_u64(tmp, vextq_u64(gh, ab, 1), vextq_u64(ef, gh, 1));
    ab = vsha512h2q_u64(tmp, ef, cd);
    ef = vaddq_u64(ef, tmp);

    /* Add original state back */
    ab = vaddq_u64(ab, ab_orig);
    cd = vaddq_u64(cd, cd_orig);
    ef = vaddq_u64(ef, ef_orig);
    gh = vaddq_u64(gh, gh_orig);

    /* Store result */
    vst1q_u64(&state[0], ab);
    vst1q_u64(&state[2], cd);
    vst1q_u64(&state[4], ef);
    vst1q_u64(&state[6], gh);
}

/* Return 1 if ARM SHA512 is available, 0 otherwise */
int sha512_arm_available(void) {
    return 1;
}

#else

/* Stub implementations when ARM SHA512 is not available */
void sha512_block_arm(uint64_t *state, const uint64_t *block) {
    (void)state;
    (void)block;
    /* Should never be called - use pure Haskell fallback */
}

int sha512_arm_available(void) {
    return 0;
}

#endif
