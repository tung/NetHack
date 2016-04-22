/* NetHack 3.6	botl.c	$NHDT-Date: 1452660188 2016/01/13 04:43:08 $  $NHDT-Branch: NetHack-3.6.0 $:$NHDT-Revision: 1.70 $ */
/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */
/* NetHack may be freely redistributed.  See license for details. */

#include "hack.h"
#include <limits.h>

extern const char *hu_stat[]; /* defined in eat.c */

const char *const enc_stat[] = { "",         "Burdened",  "Stressed",
                                 "Strained", "Overtaxed", "Overloaded" };

STATIC_OVL NEARDATA int mrank_sz = 0; /* loaded by max_rank_sz (from u_init) */
STATIC_DCL const char *NDECL(rank);

#ifndef STATUS_VIA_WINDOWPORT

STATIC_DCL void NDECL(bot1);
STATIC_DCL void NDECL(bot2);

STATIC_OVL void
bot1()
{
    char newbot1[MAXCO];
    register char *nb;
    register int i, j;

    Strcpy(newbot1, plname);
    if ('a' <= newbot1[0] && newbot1[0] <= 'z')
        newbot1[0] += 'A' - 'a';
    newbot1[10] = 0;
    Sprintf(nb = eos(newbot1), " the ");

    if (Upolyd) {
        char mbot[BUFSZ];
        int k = 0;

        Strcpy(mbot, mons[u.umonnum].mname);
        while (mbot[k] != 0) {
            if ((k == 0 || (k > 0 && mbot[k - 1] == ' ')) && 'a' <= mbot[k]
                && mbot[k] <= 'z')
                mbot[k] += 'A' - 'a';
            k++;
        }
        Strcpy(nb = eos(nb), mbot);
    } else
        Strcpy(nb = eos(nb), rank());

    Sprintf(nb = eos(nb), "  ");
    i = mrank_sz + 15;
    j = (int) ((nb + 2) - newbot1); /* strlen(newbot1) but less computation */
    if ((i - j) > 0)
        Sprintf(nb = eos(nb), "%*s", i - j, " "); /* pad with spaces */
    if (ACURR(A_STR) > 18) {
        if (ACURR(A_STR) > STR18(100))
            Sprintf(nb = eos(nb), "St:%2d ", ACURR(A_STR) - 100);
        else if (ACURR(A_STR) < STR18(100))
            Sprintf(nb = eos(nb), "St:18/%02d ", ACURR(A_STR) - 18);
        else
            Sprintf(nb = eos(nb), "St:18/** ");
    } else
        Sprintf(nb = eos(nb), "St:%-1d ", ACURR(A_STR));
    Sprintf(nb = eos(nb), "Dx:%-1d Co:%-1d In:%-1d Wi:%-1d Ch:%-1d",
            ACURR(A_DEX), ACURR(A_CON), ACURR(A_INT), ACURR(A_WIS),
            ACURR(A_CHA));
    Sprintf(nb = eos(nb),
            (u.ualign.type == A_CHAOTIC)
                ? "  Chaotic"
                : (u.ualign.type == A_NEUTRAL) ? "  Neutral" : "  Lawful");
#ifdef SCORE_ON_BOTL
    if (flags.showscore)
        Sprintf(nb = eos(nb), " S:%ld", botl_score());
#endif
    curs(WIN_STATUS, 1, 0);
    putstr(WIN_STATUS, 0, newbot1);
}

STATIC_OVL void
bot2()
{
    char newbot2[MAXCO], /* MAXCO: botl.h */
         /* dungeon location (and gold), hero health (HP, PW, AC),
            experience (HD if poly'd, else Exp level and maybe Exp points),
            time (in moves), varying number of status conditions */
         dloc[QBUFSZ], hlth[QBUFSZ], expr[QBUFSZ], tmmv[QBUFSZ], cond[QBUFSZ];
    register char *nb;
    unsigned dln, dx, hln, xln, tln, cln;
    int hp, hpmax, cap;
    long money;

    /*
     * Various min(x,9999)'s are to avoid having excessive values
     * violate the field width assumptions in botl.h and should not
     * impact normal play.  Particularly 64-bit long for gold which
     * could require many more digits if someone figures out a way
     * to get and carry a really large (or negative) amount of it.
     * Turn counter is also long, but we'll risk that.
     */

    /* dungeon location plus gold */
    (void) describe_level(dloc); /* includes at least one trailing space */
    if ((money = money_cnt(invent)) < 0L)
        money = 0L; /* ought to issue impossible() and then discard gold */
    Sprintf(eos(dloc), "%s:%-2ld", /* strongest hero can lift ~300000 gold */
            encglyph(objnum_to_glyph(GOLD_PIECE)), min(money, 999999L));
    dln = strlen(dloc);
    /* '$' encoded as \GXXXXNNNN is 9 chars longer than display will need */
    dx = strstri(dloc, "\\G") ? 9 : 0;

    /* health and armor class (has trailing space for AC 0..9) */
    hp = Upolyd ? u.mh : u.uhp;
    hpmax = Upolyd ? u.mhmax : u.uhpmax;
    if (hp < 0)
        hp = 0;
    Sprintf(hlth, "HP:%d(%d) Pw:%d(%d) AC:%-2d",
            min(hp, 9999), min(hpmax, 9999),
            min(u.uen, 9999), min(u.uenmax, 9999), u.uac);
    hln = strlen(hlth);

    /* experience */
    if (Upolyd)
        Sprintf(expr, "HD:%d", mons[u.umonnum].mlevel);
    else if (flags.showexp)
        Sprintf(expr, "Xp:%u/%-1ld", u.ulevel, u.uexp);
    else
        Sprintf(expr, "Exp:%u", u.ulevel);
    xln = strlen(expr);

    /* time/move counter */
    if (flags.time)
        Sprintf(tmmv, "T:%ld", moves);
    else
        tmmv[0] = '\0';
    tln = strlen(tmmv);

    /* status conditions; worst ones first */
    cond[0] = '\0'; /* once non-empty, cond will have a leading space */
    nb = cond;
    /*
     * Stoned, Slimed, Strangled, and both types of Sick are all fatal
     * unless remedied before timeout expires.  Should we order them by
     * shortest time left?  [Probably not worth the effort, since it's
     * unusual for more than one of them to apply at a time.]
     */
    if (Stoned)
        Strcpy(nb = eos(nb), " Stone");
    if (Slimed)
        Strcpy(nb = eos(nb), " Slime");
    if (Strangled)
        Strcpy(nb = eos(nb), " Strngl");
    if (Sick) {
        if (u.usick_type & SICK_VOMITABLE)
            Strcpy(nb = eos(nb), " FoodPois");
        if (u.usick_type & SICK_NONVOMITABLE)
            Strcpy(nb = eos(nb), " TermIll");
    }
    if (u.uhs != NOT_HUNGRY)
        Sprintf(nb = eos(nb), " %s", hu_stat[u.uhs]);
    if ((cap = near_capacity()) > UNENCUMBERED)
        Sprintf(nb = eos(nb), " %s", enc_stat[cap]);
    if (Blind)
        Strcpy(nb = eos(nb), " Blind");
    if (Deaf)
        Strcpy(nb = eos(nb), " Deaf");
    if (Stunned)
        Strcpy(nb = eos(nb), " Stun");
    if (Confusion)
        Strcpy(nb = eos(nb), " Conf");
    if (Hallucination)
        Strcpy(nb = eos(nb), " Hallu");
    /* levitation and flying are mutually exclusive; riding is not */
    if (Levitation)
        Strcpy(nb = eos(nb), " Lev");
    if (Flying)
        Strcpy(nb = eos(nb), " Fly");
    if (u.usteed)
        Strcpy(nb = eos(nb), " Ride");
    cln = strlen(cond);

    /*
     * Put the pieces together.  If they all fit, keep the traditional
     * sequence.  Otherwise, move least important parts to the end in
     * case the interface side of things has to truncate.  Note that
     * dloc[] contains '$' encoded in ten character sequence \GXXXXNNNN
     * so we want to test its display length rather than buffer length.
     *
     * We don't have an actual display limit here, so have to go by the
     * width of the map.  Since we're reordering rather than truncating,
     * wider displays can still show wider status than the map if the
     * interface supports that.
     */
    if ((dln - dx) + 1 + hln + 1 + xln + 1 + tln + 1 + cln <= COLNO) {
        Sprintf(newbot2, "%s %s %s %s %s", dloc, hlth, expr, tmmv, cond);
    } else {
        if (dln + 1 + hln + 1 + xln + 1 + tln + 1 + cln + 1 > MAXCO) {
            panic("bot2: second status line exceeds MAXCO (%u > %d)",
                  (dln + 1 + hln + 1 + xln + 1 + tln + 1 + cln + 1), MAXCO);
        } else if ((dln - dx) + 1 + hln + 1 + xln + 1 + cln <= COLNO) {
            Sprintf(newbot2, "%s %s %s %s %s", dloc, hlth, expr, cond, tmmv);
        } else if ((dln - dx) + 1 + hln + 1 + cln <= COLNO) {
            Sprintf(newbot2, "%s %s %s %s %s", dloc, hlth, cond, expr, tmmv);
        } else {
            Sprintf(newbot2, "%s %s %s %s %s", hlth, cond, dloc, expr, tmmv);
        }
        /* only two or three consecutive spaces available to squeeze out */
        mungspaces(newbot2);
    }

    curs(WIN_STATUS, 1, 1);
    putmixed(WIN_STATUS, 0, newbot2);
}

void
bot()
{
    if (youmonst.data) {
        bot1();
        bot2();
    }
    context.botl = context.botlx = 0;
}

#endif /* !STATUS_VIA_WINDOWPORT */

/* convert experience level (1..30) to rank index (0..8) */
int
xlev_to_rank(xlev)
int xlev;
{
    return (xlev <= 2) ? 0 : (xlev <= 30) ? ((xlev + 2) / 4) : 8;
}

#if 0 /* not currently needed */
/* convert rank index (0..8) to experience level (1..30) */
int
rank_to_xlev(rank)
int rank;
{
    return (rank <= 0) ? 1 : (rank <= 8) ? ((rank * 4) - 2) : 30;
}
#endif

const char *
rank_of(lev, monnum, female)
int lev;
short monnum;
boolean female;
{
    register const struct Role *role;
    register int i;

    /* Find the role */
    for (role = roles; role->name.m; role++)
        if (monnum == role->malenum || monnum == role->femalenum)
            break;
    if (!role->name.m)
        role = &urole;

    /* Find the rank */
    for (i = xlev_to_rank((int) lev); i >= 0; i--) {
        if (female && role->rank[i].f)
            return role->rank[i].f;
        if (role->rank[i].m)
            return role->rank[i].m;
    }

    /* Try the role name, instead */
    if (female && role->name.f)
        return role->name.f;
    else if (role->name.m)
        return role->name.m;
    return "Player";
}

STATIC_OVL const char *
rank()
{
    return rank_of(u.ulevel, Role_switch, flags.female);
}

int
title_to_mon(str, rank_indx, title_length)
const char *str;
int *rank_indx, *title_length;
{
    register int i, j;

    /* Loop through each of the roles */
    for (i = 0; roles[i].name.m; i++)
        for (j = 0; j < 9; j++) {
            if (roles[i].rank[j].m
                && !strncmpi(str, roles[i].rank[j].m,
                             strlen(roles[i].rank[j].m))) {
                if (rank_indx)
                    *rank_indx = j;
                if (title_length)
                    *title_length = strlen(roles[i].rank[j].m);
                return roles[i].malenum;
            }
            if (roles[i].rank[j].f
                && !strncmpi(str, roles[i].rank[j].f,
                             strlen(roles[i].rank[j].f))) {
                if (rank_indx)
                    *rank_indx = j;
                if (title_length)
                    *title_length = strlen(roles[i].rank[j].f);
                return (roles[i].femalenum != NON_PM) ? roles[i].femalenum
                                                      : roles[i].malenum;
            }
        }
    return NON_PM;
}

void
max_rank_sz()
{
    register int i, r, maxr = 0;
    for (i = 0; i < 9; i++) {
        if (urole.rank[i].m && (r = strlen(urole.rank[i].m)) > maxr)
            maxr = r;
        if (urole.rank[i].f && (r = strlen(urole.rank[i].f)) > maxr)
            maxr = r;
    }
    mrank_sz = maxr;
    return;
}

#ifdef SCORE_ON_BOTL
long
botl_score()
{
    long deepest = deepest_lev_reached(FALSE);
    long utotal;

    utotal = money_cnt(invent) + hidden_gold();
    if ((utotal -= u.umoney0) < 0L)
        utotal = 0L;
    utotal += u.urexp + (50 * (deepest - 1))
          + (deepest > 30 ? 10000 : deepest > 20 ? 1000 * (deepest - 20) : 0);
    if (utotal < u.urexp)
        utotal = LONG_MAX; /* wrap around */
    return utotal;
}
#endif /* SCORE_ON_BOTL */

/* provide the name of the current level for display by various ports */
int
describe_level(buf)
char *buf;
{
    int ret = 1;

    /* TODO:    Add in dungeon name */
    if (Is_knox(&u.uz))
        Sprintf(buf, "%s ", dungeons[u.uz.dnum].dname);
    else if (In_quest(&u.uz))
        Sprintf(buf, "Home %d ", dunlev(&u.uz));
    else if (In_endgame(&u.uz))
        Sprintf(buf, Is_astralevel(&u.uz) ? "Astral Plane " : "End Game ");
    else {
        /* ports with more room may expand this one */
        Sprintf(buf, "Dlvl:%-2d ", depth(&u.uz));
        ret = 0;
    }
    return ret;
}

#ifdef STATUS_VIA_WINDOWPORT
/* =======================================================================*/

#ifdef STATUS_COLORS

struct num_rule {
    long val;
    struct status_color_attr ca;
    struct num_rule *next;
};

struct num_ruleset {
    struct num_rule *up, *down;      /* descending order */
    struct num_rule *equal;          /* ascending order */
    struct num_rule *less_than;      /* ascending order */
    struct num_rule *percent;        /* ascending order */
    struct num_rule *greater_than;   /* descending order */
};

/*
 * There are a lot of condition rules, so they're organized into sub-menus to
 * make them easier to navigate.  Condition rules that share a str_rule_menu_id
 * value show up in the same sub-menu.  The top-level condition menu should
 * provide options to enter sub-menus.
 */
enum str_rule_menu_id {
    SRM_ANY = -1,   /* special value for functions to consider all rules */
    SRM_NONE = 0,   /* guarantee other values are non-zero for menu API */
    SRM_HUNGER,
    SRM_BURDEN,
    MAX_SRM
};

static const char * const str_rule_menu_headings[MAX_SRM] = {
    (const char *) 0,
    "Hunger...",
    "Burden...",
};

struct str_rule {
    const char *val;
    struct status_color_attr ca;
    boolean active;
    enum str_rule_menu_id menu;
};

enum num_ruleset_field_id {
    NRSF_UP, NRSF_DOWN,
    NRSF_EQUAL,
    NRSF_LESS_THAN,
    NRSF_PERCENT,
    NRSF_GREATER_THAN,
    MAX_NRSF
};

enum num_ruleset_id {
    NRS_ST, NRS_DX, NRS_CO, NRS_IN, NRS_WI, NRS_CH,
    NRS_SCORE,
    NRS_GOLD,
    NRS_HP,
    NRS_PW,
    NRS_AC,
    NRS_EXP_LEVEL, NRS_EXP_POINTS,
    NRS_TURNS,
    MAX_NRS
};

#define MAX_COND_RULES 18

#define EMPTY_NUM_RULESET { \
        (struct num_rule *) 0, \
        (struct num_rule *) 0, \
        (struct num_rule *) 0, \
        (struct num_rule *) 0, \
        (struct num_rule *) 0, \
        (struct num_rule *) 0, \
    }
#define EMPTY_CA { 0, 0 }

static struct {
    struct num_ruleset st, dx, co, in, wi, ch;
    struct num_ruleset score;

    struct num_ruleset gold;
    struct num_ruleset hp;
    struct num_ruleset pw;
    struct num_ruleset ac;
    struct num_ruleset exp_level, exp_points;
    struct num_ruleset turns;

    struct str_rule conds[MAX_COND_RULES];
} status_color_rules = {
    EMPTY_NUM_RULESET,
    EMPTY_NUM_RULESET,
    EMPTY_NUM_RULESET,
    EMPTY_NUM_RULESET,
    EMPTY_NUM_RULESET,
    EMPTY_NUM_RULESET,
    EMPTY_NUM_RULESET,

    EMPTY_NUM_RULESET,
    EMPTY_NUM_RULESET,
    EMPTY_NUM_RULESET,
    EMPTY_NUM_RULESET,
    EMPTY_NUM_RULESET,
    EMPTY_NUM_RULESET,
    EMPTY_NUM_RULESET,

    {
        { "Blind", EMPTY_CA, FALSE, SRM_NONE },
        { "Conf", EMPTY_CA, FALSE, SRM_NONE },
        { "FoodPois", EMPTY_CA, FALSE, SRM_NONE },
        { "Hallu", EMPTY_CA, FALSE, SRM_NONE },
        { "Ill", EMPTY_CA, FALSE, SRM_NONE },
        { "Slime", EMPTY_CA, FALSE, SRM_NONE },
        { "Stun", EMPTY_CA, FALSE, SRM_NONE },

        { "Satiated", EMPTY_CA, FALSE, SRM_HUNGER },
        { "Hungry", EMPTY_CA, FALSE, SRM_HUNGER },
        { "Weak", EMPTY_CA, FALSE, SRM_HUNGER },
        { "Fainting", EMPTY_CA, FALSE, SRM_HUNGER },
        { "Fainted", EMPTY_CA, FALSE, SRM_HUNGER },
        { "Starved", EMPTY_CA, FALSE, SRM_HUNGER },

        { "Burdened", EMPTY_CA, FALSE, SRM_BURDEN },
        { "Stressed", EMPTY_CA, FALSE, SRM_BURDEN },
        { "Strained", EMPTY_CA, FALSE, SRM_BURDEN },
        { "Overtaxed", EMPTY_CA, FALSE, SRM_BURDEN },
        { "Overloaded", EMPTY_CA, FALSE, SRM_BURDEN },
    }
};

static int which_status = -1;

#endif /* STATUS_COLORS */

static boolean blinit = FALSE;

#ifdef STATUS_COLORS

/* Meta-data maps and mapping functions. */
STATIC_DCL struct num_rule **FDECL(get_nrs_field,
        (enum num_ruleset_id, enum num_ruleset_field_id));
STATIC_DCL long FDECL(get_status_info_field,
        (const struct status_info *, enum num_ruleset_id));
STATIC_DCL long FDECL(get_status_info_field_max,
        (const struct status_info *, enum num_ruleset_id));
STATIC_DCL struct status_color_attr * FDECL(get_status_color_attr,
        (struct status_info_colors *, enum num_ruleset_id));

/* Book-keeping functions. */
STATIC_DCL boolean FDECL(add_num_rule,
        (enum num_ruleset_id, enum num_ruleset_field_id, long,
         const struct status_color_attr *, BOOLEAN_P));
STATIC_DCL boolean FDECL(add_str_rule,
        (struct str_rule *, int, const char *, const struct status_color_attr *,
         BOOLEAN_P));
STATIC_DCL void FDECL(remove_num_rule,
        (enum num_ruleset_id, enum num_ruleset_field_id, int));
STATIC_DCL void FDECL(remove_str_rule, (struct str_rule *, int, const char *));
STATIC_DCL void FDECL(free_num_rules, (struct num_rule *));
STATIC_DCL void NDECL(free_rules);

/* Utility functions. */
STATIC_DCL int FDECL(count_num_rules, (const struct num_rule *));
STATIC_DCL int FDECL(count_num_ruleset, (enum num_ruleset_id));
STATIC_DCL int FDECL(count_str_rules,
        (const struct str_rule *, int, enum str_rule_menu_id srm_id));
STATIC_DCL int FDECL(strength_from_parts, (int, int));
STATIC_DCL void FDECL(str_from_status_color_attr,
        (char *, const struct status_color_attr *));
STATIC_DCL boolean FDECL(status_num_from_str,
        (const char *, long *, enum num_ruleset_id, enum num_ruleset_field_id));
STATIC_DCL void FDECL(str_from_status_num,
        (char *, long, enum num_ruleset_id, enum num_ruleset_field_id));
STATIC_DCL void FDECL(str_from_num_rule,
        (char *, enum num_ruleset_id, enum num_ruleset_field_id,
         const struct num_rule *));
STATIC_DCL void FDECL(str_from_str_rule, (char *, const struct str_rule *));

/* STATUSCOLOR option parsing. */
STATIC_DCL boolean FDECL(parse_color_attr,
        (char *, struct status_color_attr *));
STATIC_DCL boolean FDECL(parse_num_rule, (char *, enum num_ruleset_id));
STATIC_DCL boolean FDECL(parse_str_rule, (char *, struct str_rule *));
STATIC_DCL boolean FDECL(parse_status_color_rule, (char *));

/* Status colors option/customization menus. */
STATIC_DCL void FDECL(menu_num_breadcrumbs,
        (char *, enum num_ruleset_id, enum num_ruleset_field_id, BOOLEAN_P,
         long, int));
STATIC_DCL void FDECL(menu_str_breadcrumbs, (char *, const char *, int));
STATIC_DCL void FDECL(display_raw_num_rule,
        (winid, enum num_ruleset_id, enum num_ruleset_field_id,
         const struct num_rule *));
STATIC_DCL void FDECL(display_raw_num_ruleset, (winid, enum num_ruleset_id));
STATIC_DCL void FDECL(display_raw_str_rules,
        (winid, const struct str_rule *, int));
STATIC_DCL void NDECL(display_raw_rules);
STATIC_DCL boolean FDECL(pick_color_menu, (int *, const char *));
STATIC_DCL boolean FDECL(pick_attr_menu, (int *, const char *));
STATIC_DCL boolean FDECL(prompt_num_color_attr,
        (struct status_color_attr *, long, enum num_ruleset_id,
         enum num_ruleset_field_id));
STATIC_DCL boolean FDECL(prompt_str_color_attr,
        (struct status_color_attr *, const char *));
STATIC_DCL void FDECL(prompt_add_num_rule,
        (enum num_ruleset_id, enum num_ruleset_field_id));
STATIC_DCL void FDECL(prompt_add_str_rule,
        (struct str_rule *, int, const char *));
STATIC_DCL void FDECL(prompt_remove_num_rule,
        (enum num_ruleset_id, enum num_ruleset_field_id, int));
STATIC_DCL void FDECL(prompt_remove_str_rule,
        (struct str_rule *, int, const char *));
STATIC_DCL void FDECL(menu_num_ruleset_field,
        (enum num_ruleset_id, enum num_ruleset_field_id));
STATIC_DCL void FDECL(menu_num_ruleset, (enum num_ruleset_id));
STATIC_DCL void FDECL(menu_str_rules, (enum str_rule_menu_id));

/* Status population and color application. */
STATIC_DCL void FDECL(apply_num_ruleset,
        (enum num_ruleset_id, long, long, long, struct status_color_attr *));
STATIC_DCL void FDECL(apply_status_colors,
        (const struct status_info *, const struct status_info *,
         struct status_info_colors *));
#endif /* STATUS_COLORS */

STATIC_DCL void FDECL(add_status_info_cond,
                      (struct status_info *, const char *));
STATIC_DCL void FDECL(populate_status_info, (struct status_info *));

#ifdef STATUS_COLORS

/*****************************************
 * Meta-data maps and mapping functions. *
 *****************************************/

static const struct {
    int order;
    char accel;
    char op;
    const char *label;
    const char *prompt;
    long min_value; /* 0 = no particular minimum value */
} num_ruleset_fields[MAX_NRSF] = {
    { -1, 'u', '+', "up", "minimum change amount", 1 },
    { -1, 'd', '-', "down", "minimum change amount", 1 },
    {  1, 'e', '.', "equal", "equal value", 0 },
    {  1, 'l', '<', "less than", "less than value", 0 },
    {  1, 'p', '%', "percent", "percentage", 0 },
    { -1, 'g', '>', "greater than", "greater than value", 0 },
};

/* Positions based on the CLR_* constants. */
static const char *color_names[CLR_MAX] = {
    "black",
    "red",
    "green",
    "brown",
    "blue",
    "magenta",
    "cyan",
    "gray",
    "nocolor",
    "orange",
    "brightgreen",
    "yellow",
    "brightblue",
    "brightmagenta",
    "brightcyan",
    "white",
};

/* Backwards compatibility with original statuscolors patch. */
#define ALT_CLR_MAX 4
static const struct {
    const char *name;
    int color;
} alt_color_names[ALT_CLR_MAX] = {
    { "lightgreen", CLR_BRIGHT_GREEN },
    { "lightblue", CLR_BRIGHT_BLUE },
    { "lightmagenta", CLR_BRIGHT_MAGENTA },
    { "lightcyan", CLR_BRIGHT_CYAN },
};

/* Positions based on ATR_* constants. */
#define ATTR_NAME_MAX (ATR_INVERSE + 1)
static const char *attr_names[ATTR_NAME_MAX] = {
    "none", /* ATR_NONE */
    "bold", /* ATR_BOLD */
    "dim", /* ATR_DIM */
    (const char *) 0,
    "underline", /* ATR_ULINE */
    "blink", /* ATR_BLINK */
    (const char *) 0,
    "inverse", /* ATR_INVERSE */
};

static struct {
    struct num_ruleset *ruleset;
    const char *label;
    boolean has_max;
} status_rulesets[MAX_NRS] = {
    { &status_color_rules.st, "St", FALSE },
    { &status_color_rules.dx, "Dx", FALSE },
    { &status_color_rules.co, "Co", FALSE },
    { &status_color_rules.in, "In", FALSE },
    { &status_color_rules.wi, "Wi", FALSE },
    { &status_color_rules.ch, "Ch", FALSE },
    { &status_color_rules.score, "Score", FALSE },
    { &status_color_rules.gold, "Gold", FALSE },
    { &status_color_rules.hp, "HP", TRUE },
    { &status_color_rules.pw, "Pw", TRUE },
    { &status_color_rules.ac, "AC", FALSE },
    { &status_color_rules.exp_level, "XL", FALSE },
    { &status_color_rules.exp_points, "XP", FALSE },
    { &status_color_rules.turns, "Turns", FALSE },
};

STATIC_OVL struct num_rule **
get_nrs_field(nrs_id, nrsf_id)
enum num_ruleset_id nrs_id;
enum num_ruleset_field_id nrsf_id;
{
    struct num_ruleset *ruleset = status_rulesets[nrs_id].ruleset;
    switch (nrsf_id) {
    case NRSF_UP:
        return &ruleset->up;
    case NRSF_DOWN:
        return &ruleset->down;
    case NRSF_EQUAL:
        return &ruleset->equal;
    case NRSF_LESS_THAN:
        return &ruleset->less_than;
    case NRSF_PERCENT:
        return &ruleset->percent;
    case NRSF_GREATER_THAN:
        return &ruleset->greater_than;
    default:
        panic("get_nrs_field: bad ID (%d)", nrsf_id);
    }
    return (struct num_rule **) 0;
}

STATIC_OVL long
get_status_info_field(si, nrs_id)
const struct status_info *si;
enum num_ruleset_id nrs_id;
{
    switch (nrs_id) {
    case NRS_ST:
        return si->st;
    case NRS_DX:
        return si->dx;
    case NRS_CO:
        return si->co;
    case NRS_IN:
        return si->in;
    case NRS_WI:
        return si->wi;
    case NRS_CH:
        return si->ch;
    case NRS_SCORE:
        return si->score;
    case NRS_GOLD:
        return si->gold;
    case NRS_HP:
        return si->hp;
    case NRS_PW:
        return si->pw;
    case NRS_AC:
        return si->ac;
    case NRS_EXP_LEVEL:
        return si->exp_level;
    case NRS_EXP_POINTS:
        return si->exp_points;
    case NRS_TURNS:
        return si->turns;
    default:
        panic("get_status_info_field: bad ID (%d)", nrs_id);
    }
    return -1;
}

STATIC_OVL long
get_status_info_field_max(si, nrs_id)
const struct status_info *si;
enum num_ruleset_id nrs_id;
{
    if (!status_rulesets[nrs_id].has_max)
        return -1;
    if (nrs_id == NRS_HP)
        return si->hp_max;
    if (nrs_id == NRS_PW)
        return si->pw_max;
    return -1;
}

STATIC_OVL struct status_color_attr *
get_status_color_attr(sic, nrs_id)
struct status_info_colors *sic;
enum num_ruleset_id nrs_id;
{
    switch (nrs_id) {
    case NRS_ST:
        return &sic->st;
    case NRS_DX:
        return &sic->dx;
    case NRS_CO:
        return &sic->co;
    case NRS_IN:
        return &sic->in;
    case NRS_WI:
        return &sic->wi;
    case NRS_CH:
        return &sic->ch;
    case NRS_SCORE:
        return &sic->score;
    case NRS_GOLD:
        return &sic->gold;
    case NRS_HP:
        return &sic->hp;
    case NRS_PW:
        return &sic->pw;
    case NRS_AC:
        return &sic->ac;
    case NRS_EXP_LEVEL:
        return &sic->exp_level;
    case NRS_EXP_POINTS:
        return &sic->exp_points;
    case NRS_TURNS:
        return &sic->turns;
    default:
        panic("get_status_color_attr: bad ID (%d)", nrs_id);
    }
    return (struct status_color_attr *) 0;
}

/***************************
 * Book-keeping functions. *
 ***************************/

STATIC_OVL boolean
add_num_rule(nrs_id, nrsf_id, val, ca, allow_replace)
enum num_ruleset_id nrs_id;
enum num_ruleset_field_id nrsf_id;
long val;
const struct status_color_attr *ca;
boolean allow_replace;
{
    struct num_rule **list = get_nrs_field(nrs_id, nrsf_id);
    int order = num_ruleset_fields[nrsf_id].order;
    struct num_rule *new_rule;
    struct num_rule *prev, *curr;

    if (order != 1 && order != -1)
        return FALSE;

    /* Find the correct insertion point for the chosen ordering. */
    prev = (struct num_rule *) 0;
    curr = *list;
    while (curr && ((order ==  1 && curr->val < val) ||
                    (order == -1 && curr->val > val))) {
        prev = curr;
        curr = curr->next;
    }

    if (curr && curr->val == val) {
        if (allow_replace) {
            curr->ca = *ca;
            return TRUE;
        } else {
            return FALSE;
        }
    }

    new_rule = (struct num_rule *) alloc(sizeof(struct num_rule));
    if (!new_rule)
        return FALSE;
    new_rule->val = val;
    new_rule->ca = *ca;

    if (prev)
        prev->next = new_rule;
    else
        *list = new_rule;

    new_rule->next = curr;

    return TRUE;
}

STATIC_OVL boolean
add_str_rule(rules, rule_size, str, ca, allow_replace)
struct str_rule *rules;
int rule_size;
const char *str;
const struct status_color_attr *ca;
boolean allow_replace;
{
    int i;

    if (!str || !str[0])
        return FALSE;

    for (i = 0; i < rule_size; i++) {
        if (!strcmpi(str, rules[i].val)) {
            if (!rules[i].active || allow_replace) {
                rules[i].ca = *ca;
                rules[i].active = TRUE;
                return TRUE;
            } else {
                return FALSE;
            }
        }
    }

    return FALSE;
}

STATIC_OVL void
remove_num_rule(nrs_id, nrsf_id, pos)
enum num_ruleset_id nrs_id;
enum num_ruleset_field_id nrsf_id;
int pos;
{
    struct num_rule **list = get_nrs_field(nrs_id, nrsf_id);
    struct num_rule *curr, *prev;

    curr = *list;
    prev = (struct num_rule *) 0;
    while (pos > 0 && curr) {
        prev = curr;
        curr = curr->next;
        pos--;
    }

    if (!curr)
        return;

    if (prev)
        prev->next = curr->next;
    else
        *list = curr->next;

    free(curr);
}

STATIC_OVL void
remove_str_rule(rules, rule_size, which)
struct str_rule *rules;
int rule_size;
const char *which;
{
    int i;
    for (i = 0; i < rule_size; i++) {
        if (!strcmpi(rules[i].val, which)) {
            rules[i].active = FALSE;
            return;
        }
    }
}

STATIC_OVL void
free_num_rules(list)
struct num_rule *list;
{
    struct num_rule *curr, *next;
    curr = list;
    while (curr) {
        next = curr->next;
        free(curr);
        curr = next;
    }
}

STATIC_OVL void
free_rules()
{
    int nrs_id, nrsf_id;

    for (nrs_id = 0; nrs_id < MAX_NRS; nrs_id++) {
        for (nrsf_id = 0; nrsf_id < MAX_NRSF; nrsf_id++) {
            free_num_rules(*get_nrs_field(nrs_id, nrsf_id));
        }
    }
}

/**********************
 * Utility functions. *
 **********************/

STATIC_OVL int
count_num_rules(list)
const struct num_rule *list;
{
    int count = 0;
    while (list) {
        count++;
        list = list->next;
    }
    return count;
}

STATIC_OVL int
count_num_ruleset(nrs_id)
enum num_ruleset_id nrs_id;
{
    int count = 0;
    int nrsf_id;
    for (nrsf_id = 0; nrsf_id < MAX_NRSF; nrsf_id++)
        count += count_num_rules(*get_nrs_field(nrs_id, nrsf_id));
    return count;
}

STATIC_OVL int
count_str_rules(rules, rule_size, srm_id)
const struct str_rule *rules;
int rule_size;
enum str_rule_menu_id srm_id;
{
    int count = 0;
    int i;
    for (i = 0; i < rule_size; i++) {
        if (srm_id != SRM_ANY && rules[i].menu != srm_id)
            continue;
        if (rules[i].active) {
            count++;
        }
    }
    return count;
}

int
count_status_colors()
{
    int count = 0;
    int nrs_id;
    for (nrs_id = 0; nrs_id < MAX_NRS; nrs_id++)
        count += count_num_ruleset(nrs_id);
    count += count_str_rules(status_color_rules.conds, MAX_COND_RULES, SRM_ANY);
    return count;
}

/* Convert strength into internal form, e.g. 18/23 into 41. */
STATIC_OVL int
strength_from_parts(st_main, st_sub)
int st_main, st_sub;
{
    if (st_main < 18)
        return st_main;
    else if (st_main >= 19)
        return STR19(st_main);
    else
        return STR18(st_sub);
}

STATIC_OVL void
str_from_status_color_attr(buf, ca)
char *buf;
const struct status_color_attr *ca;
{
    int i;
    Strcpy(buf, color_names[ca->color]);
    for (i = 0; i < ATTR_NAME_MAX; i++) {
        if (attr_names[i] && (ca->attr_bits & (1 << i))) {
            Strcat(buf, "&");
            Strcat(buf, attr_names[i]);
        }
    }
}

/* Convert a string into a value.  This is able to understand absolute values
 * of strength and translate them into internal form, e.g. 18/xx.
 */
STATIC_OVL boolean
status_num_from_str(buf, val, nrs_id, nrsf_id)
const char *buf;
long *val;
enum num_ruleset_id nrs_id;
enum num_ruleset_field_id nrsf_id;
{
    boolean translate_abs_st = (nrs_id == NRS_ST
                                && (nrsf_id == NRSF_EQUAL
                                    || nrsf_id == NRSF_LESS_THAN
                                    || nrsf_id == NRSF_GREATER_THAN));
    char *tmp;

    *val = strtol(buf, &tmp, 10);
    if (tmp[0]) {
        if (!translate_abs_st || *val != 18 || tmp[0] != '/')
            return FALSE;

        /* This is a string in the form of "18/xx"; interpret the "xx" bit. */
        tmp++; /* Skip the slash. */
        if (!strcmp(tmp, "**")) {
            /* "18/xx" where "xx" is "**"  */
            *val += 100;
            return TRUE;
        } else if (tmp[0] && tmp[1]
                   && (tmp[0] >= '0' && tmp[0] <= '9')
                   && (tmp[1] >= '0' && tmp[1] <= '9')) {
            /* "18/dd" where 'd' is a digit */
            *val += (tmp[0] - '0') * 10 + (tmp[1] - '0');
            return TRUE;
        } else {
            /* Malformed "18/xx" value. */
            return FALSE;
        }
    }

    if (translate_abs_st && *val >= 19)
        *val = STR19(*val);

    return TRUE;
}

/* Convert a value into a string.  This is able to translate absolute strength
 * values from internal form to their player-friendly format. */
STATIC_OVL void
str_from_status_num(buf, val, nrs_id, nrsf_id)
char *buf;
long val;
enum num_ruleset_id nrs_id;
enum num_ruleset_field_id nrsf_id;
{
    if (nrs_id == NRS_ST && (nrsf_id == NRSF_EQUAL
                             || nrsf_id == NRSF_LESS_THAN
                             || nrsf_id == NRSF_GREATER_THAN)) {
        /* This is an absolute strength value, so translate it. */
        if (val <= 18)
            Sprintf(buf, "%ld", val);
        else if (val < STR18(100))
            Sprintf(buf, "18/%02ld", val - 18);
        else if (val == STR18(100))
            Sprintf(buf, "18/**");
        else
            Sprintf(buf, "%ld", val - 100);
    } else {
        Sprintf(buf, "%ld", val);
    }
}

/* Write a numeric rule to buf, e.g. "HP%100:green" */
STATIC_OVL void
str_from_num_rule(buf, nrs_id, nrsf_id, rule)
char *buf;
enum num_ruleset_id nrs_id;
enum num_ruleset_field_id nrsf_id;
const struct num_rule *rule;
{
    Sprintf(buf, "%s%c", status_rulesets[nrs_id].label,
            num_ruleset_fields[nrsf_id].op);
    str_from_status_num(eos(buf), rule->val, nrs_id, nrsf_id);
    Strcat(buf, ":");
    str_from_status_color_attr(eos(buf), &rule->ca);
}

/* Write a string rule to buf, e.g. "Hungry:yellow" */
STATIC_OVL void
str_from_str_rule(buf, rule)
char *buf;
const struct str_rule *rule;
{
    Strcpy(buf, rule->val);
    Strcat(buf, ":");
    str_from_status_color_attr(eos(buf), &rule->ca);
}

/*******************************
 * STATUSCOLOR option parsing. *
 *******************************/

/* Parse a color and optional attributes like "yellow&underline". */
STATIC_OVL boolean
parse_color_attr(start, ca)
char *start;
struct status_color_attr *ca;
{
    int color;
    int attr_bits = 0;
    char *next;
    int i;

    /* Get the first ampersand after the color. */
    next = strchr(start, '&');
    if (next) {
        /* Null terminate the color. */
        *next = '\0';
        next++;
        /* Ampersands must be immediately followed with an attribute. */
        if (!((*next >= 'A' && *next <= 'Z') || (*next >= 'a' && *next <= 'z')))
            return FALSE;
    } else {
        /* Make attribute parsing more convenient. */
        next = start;
        while (*next)
            next++;
    }

    /* Parse the color. */
    for (i = 0; i < CLR_MAX; i++) {
        if (!strcmpi(start, color_names[i])) {
            color = i;
            break;
        }
    }

    /* Check alternate color names if needed. */
    if (i >= CLR_MAX) {
        for (i = 0; i < ALT_CLR_MAX; i++) {
            if (!strcmpi(start, alt_color_names[i].name)) {
                color = alt_color_names[i].color;
                break;
            }
        }
        /* Color name not found. */
        if (i >= ALT_CLR_MAX)
            return FALSE;
    }

    /* Parse attributes separated by ampersands, if any. */
    start = next;
    while (*start) {
        /* Find next ampersand, if any. */
        next = strchr(start, '&');
        if (next) {
            /* Null terminate this attribute name. */
            *next = '\0';
            next++;
            /* Ampersands must be immediately followed with an attribute. */
            if (!((*next >= 'A' && *next <= 'Z')
                  || (*next >= 'a' && *next <= 'z')))
                return FALSE;
        } else {
            next = start;
            while (*next)
                next++;
        }

        /* Parse the attribute name. */
        for (i = 0; i < ATTR_NAME_MAX; i++) {
            if (attr_names[i] && !strcmpi(start, attr_names[i])) {
                /* Don't allow duplicate attributes. */
                if (attr_bits & (1 << i))
                    return FALSE;
                attr_bits |= (1 << i);
                break;
            }
        }
        /* Attribute name not found. */
        if (i >= ATTR_NAME_MAX)
            return FALSE;

        start = next;
    }

    ca->color = color;
    ca->attr_bits = attr_bits;
    return TRUE;
}

/* Parse a numeric rule like "HP%100:green" */
STATIC_OVL boolean
parse_num_rule(str, nrs_id)
char *str;
enum num_ruleset_id nrs_id;
{
    char ops[MAX_NRSF + 1];
    char *ca_start;
    char *op;
    int i;
    enum num_ruleset_field_id nrsf_id;
    char *val_start;
    long val;
    struct status_color_attr ca;

    /* Get the color and attr position before anything else. */
    ca_start = strpbrk(str, ":=");
    if (ca_start) {
        /* Skip the colon/equals and split off the color and attrs. */
        *ca_start = '\0';
        ca_start++;
    } else {
        return FALSE;
    }

    /* Identify operator and split label and value. */
    /* e.g. Find '%' in "HP%100" and split into "HP" and "100". */
    for (i = 0; i < MAX_NRSF; i++)
        ops[i] = num_ruleset_fields[i].op;
    ops[MAX_NRSF] = '\0';
    op = strpbrk(str, ops);
    if (op) {
        /* Identify the operator. */
        for (i = 0; i < MAX_NRSF; i++) {
            if (*op == num_ruleset_fields[i].op) {
                nrsf_id = i;
                break;
            }
        }
        /* Split the label and value here. */
        *op = '\0';
        val_start = op + 1;
    } else {
        return FALSE;
    }

    /* Verify the label is what was expected going into this. */
    if (strcmpi(str, status_rulesets[nrs_id].label))
        return FALSE;

    /* Parse the value. */
    if (!status_num_from_str(val_start, &val, nrs_id, nrsf_id))
        return FALSE;

    /* Parse color and attrs. */
    if (!parse_color_attr(ca_start, &ca))
        return FALSE;

    if (!add_num_rule(nrs_id, nrsf_id, val, &ca, FALSE))
        return FALSE;

    return TRUE;
}

/* Parse a string rule like "Hungry:yellow&underline." */
STATIC_OVL boolean
parse_str_rule(str, rule)
char *str;
struct str_rule *rule;
{
    char *ca_start;
    struct status_color_attr ca;

    /* Split the rule string from the color and attrs. */
    ca_start = strpbrk(str, ":=");
    if (ca_start) {
        *ca_start = '\0';
        ca_start++;
    } else {
        return FALSE;
    }

    /* Verify that the rule strings match. */
    if (strcmpi(str, rule->val))
        return FALSE;

    /* Parse color and attrs. */
    if (!parse_color_attr(ca_start, &ca))
        return FALSE;

    rule->active = TRUE;
    rule->ca = ca;

    return TRUE;
}

/* Parse a rule like "HP%100:green" or "Hungry:yellow&underline". */
STATIC_OVL boolean
parse_status_color_rule(start)
char *start;
{
    const char *label;
    size_t label_len;
    int i;

    /* Is this a numeric rule? */
    for (i = 0; i < MAX_NRS; i++) {
        label = status_rulesets[i].label;
        label_len = strlen(label);
        if (!strncmpi(start, label, label_len)) {
            /* Prevent false matches based on prefix. */
            if ((start[label_len] >= 'A' && start[label_len] <= 'Z')
                || (start[label_len] >= 'a' && start[label_len] <= 'z'))
                continue;
            return parse_num_rule(start, i);
        }
    }

    /* Is this a condition rule? */
    for (i = 0; i < MAX_COND_RULES; i++) {
        struct str_rule *rule = &status_color_rules.conds[i];
        label = rule->val;
        label_len = strlen(label);
        if (!strncmpi(start, label, label_len)) {
            /* Prevent false matches based on prefix. */
            if (start[label_len] != ':')
                continue;
            return parse_str_rule(start, rule);
        }
    }

    return FALSE;
}

/* Parse an option line like "HP%100:green,Hungry:yellow&underline". */
boolean
parse_status_color_line(start)
char *start;
{
    char *end, *next;
    boolean ok = TRUE;

    while (*start) {
        /* Skip leading spaces. */
        while (*start == ' ' && *start != '\0')
            start++;

        /* Find next comma, if any. */
        end = start;
        while (*end != ',' && *end != '\0')
            end++;

        /* Save the starting point for the next rule, if any. */
        next = end;
        if (*next != '\0')
            next++;

        /* Omit trailing spaces. */
        while (end > start && *(end - 1) == ' ')
            end--;

        /* Treat comma-delimited rules as separate strings. */
        *end = '\0';
        ok = parse_status_color_rule(start) && ok;

        /* Handle the next rule. */
        start = next;
    }

    return ok;
}

/*********************************************
 * Status colors option/customization menus. *
 *********************************************/

/* Put together the numeric rule menu/prompt prefix,
 * e.g. "[HP | (%) percent | 100 | green]"
 */
STATIC_OVL void
menu_num_breadcrumbs(buf, nrs_id, nrsf_id, show_val, val, color)
char *buf;
enum num_ruleset_id nrs_id;
enum num_ruleset_field_id nrsf_id;
boolean show_val;
long val;
int color;
{
    Sprintf(buf, "[%s", status_rulesets[nrs_id].label);
    if (nrsf_id < MAX_NRSF) {
        Sprintf(eos(buf), " | (%c) %s", num_ruleset_fields[nrsf_id].op,
                num_ruleset_fields[nrsf_id].label);
    }
    if (show_val) {
        Strcat(buf, " | ");
        str_from_status_num(eos(buf), val, nrs_id, nrsf_id);
    }
    if (color < CLR_MAX)
        Sprintf(eos(buf), " | %s", color_names[color]);
    Strcat(buf, "]");
}

/* Put together the string rule menu/prompt prefix,
 * e.g. "[Conditions | Hungry | yellow]"
 */
STATIC_OVL void
menu_str_breadcrumbs(buf, str, color)
char *buf;
const char *str;
int color;
{
    Strcpy(buf, "[Conditions");
    if (str) {
        Strcat(buf, " | ");
        Strcat(buf, str);
    }
    if (color < CLR_MAX)
        Sprintf(eos(buf), " | %s", color_names[color]);
    Strcat(buf, "]");
}

/* Add e.g. STATUSCOLOR=HP%100:green to a text window. */
STATIC_OVL void
display_raw_num_rule(win, nrs_id, nrsf_id, rule)
winid win;
enum num_ruleset_id nrs_id;
enum num_ruleset_field_id nrsf_id;
const struct num_rule *rule;
{
    char buf[BUFSZ];
    Strcpy(buf, "STATUSCOLOR=");
    str_from_num_rule(eos(buf), nrs_id, nrsf_id, rule);
    putstr(win, 0, buf);
}

/* Add all rules belonging to a ruleset (e.g. HP) to a text window. */
STATIC_OVL void
display_raw_num_ruleset(win, nrs_id)
winid win;
enum num_ruleset_id nrs_id;
{
    int i;

    for (i = 0; i < MAX_NRSF; i++) {
        const struct num_rule *curr = *get_nrs_field(nrs_id, i);
        while (curr) {
            display_raw_num_rule(win, nrs_id, i, curr);
            curr = curr->next;
        }
    }
}

/* Add e.g. STATUSCOLOR=Hungry:yellow to a text window. */
STATIC_OVL void
display_raw_str_rules(win, rules, rule_size)
winid win;
const struct str_rule *rules;
int rule_size;
{
    char buf[BUFSZ];
    int i;

    for (i = 0; i < rule_size; i++) {
        if (rules[i].active) {
            Strcpy(buf, "STATUSCOLOR=");
            str_from_str_rule(eos(buf), &rules[i]);
            putstr(win, 0, buf);
        }
    }
}

/* Display STATUSCOLOR rules in a text window for easy copying and pasting
 * into a config file.
 */
STATIC_OVL void
display_raw_rules()
{
    winid rulewin;
    int num_rules, i;
    char buf[BUFSZ];

    num_rules = count_status_colors();

    if (num_rules == 0) {
        pline("No statuscolors defined yet!");
        return;
    }

    rulewin = create_nhwindow(NHW_TEXT);

    Sprintf(buf, "# statuscolors: %d rule%s defined.", num_rules,
            num_rules == 1 ? "" : "s");
    putstr(rulewin, 0, buf);

    for (i = 0; i < MAX_NRS; i++)
        display_raw_num_ruleset(rulewin, i);

    display_raw_str_rules(rulewin, status_color_rules.conds, MAX_COND_RULES);

    display_nhwindow(rulewin, TRUE);
    destroy_nhwindow(rulewin);
}

/* Display the "Select color" menu.  The chosen color is stored in color.
 * prefix will be shown before the prompt to "Select color:".  Return TRUE if a
 * color was picked, FALSE if the menu was escaped.
 */
STATIC_OVL boolean
pick_color_menu(color, prefix)
int *color;
const char *prefix;
{
    winid menu;
    anything any;
    menu_item *picks;
    int num_picks, i;
    char buf[BUFSZ];

    menu = create_nhwindow(NHW_MENU);
    start_menu(menu);

    /* Put "nocolor" first because it doesn't really fit amongst the colors. */
    any.a_int = NO_COLOR + 1;
    add_menu(menu, NO_GLYPH, &any, 0, 0, ATR_NONE, "nocolor", MENU_UNSELECTED);
    for (i = 0; i < CLR_MAX; i++) {
        if (i == NO_COLOR)
            continue;
        any.a_int = i + 1;
        add_menu(menu, NO_GLYPH, &any, 0, 0, ATR_NONE, color_names[i],
                 MENU_UNSELECTED);
    }

    Strcpy(buf, prefix);
    Strcat(buf, " Select color:");
    end_menu(menu, buf);

    num_picks = select_menu(menu, PICK_ONE, &picks);
    destroy_nhwindow(menu);
    if (num_picks < 1)
        return FALSE;
    *color = picks[0].item.a_int - 1;
    free(picks);
    return TRUE;
}

/* Display the "Select attribute(s)" menu.  Multiple attributes can be chosen.
 * Return TRUE if selections were confirmed with Space/Enter/etc., or FALSE
 * otherwise.
 */
STATIC_OVL boolean
pick_attr_menu(attr_bits, prefix)
int *attr_bits;
const char *prefix;
{
    winid menu;
    anything any;
    menu_item *picks;
    int num_picks, i;
    char buf[BUFSZ];

    menu = create_nhwindow(NHW_MENU);
    start_menu(menu);

    for (i = 0; i < ATTR_NAME_MAX; i++) {
        /* Don't show "none"; it does nothing and is misleading as an option. */
        if (!attr_names[i] || i == ATR_NONE)
            continue;
        any.a_int = i + 1;
        add_menu(menu, NO_GLYPH, &any, 0, 0, ATR_NONE, attr_names[i],
                 MENU_UNSELECTED);
    }

    Strcpy(buf, prefix);
    Strcat(buf, " Select attribute(s):");
    end_menu(menu, buf);

    num_picks = select_menu(menu, PICK_ANY, &picks);
    destroy_nhwindow(menu);
    if (num_picks < 0) {
        return FALSE;
    } else if (num_picks == 0) {
        *attr_bits = 0;
    } else {
        *attr_bits = 0;
        for (i = 0; i < num_picks; i++)
            *attr_bits |= (1 << (picks[i].item.a_int - 1));
        free(picks);
    }
    return TRUE;
}

/* Prompt for color and attributes for a new num_rule.  If neither menu is
 * cancelled, the color and attributes are stored in ca and TRUE is returned.
 * If either menu is cancelled, ca remains unaltered and FALSE is returned
 * instead.
 */
STATIC_OVL boolean
prompt_num_color_attr(ca, val, nrs_id, nrsf_id)
struct status_color_attr *ca;
long val;
enum num_ruleset_id nrs_id;
enum num_ruleset_field_id nrsf_id;
{
    int color;
    int attr_bits;
    char buf[BUFSZ];

    menu_num_breadcrumbs(buf, nrs_id, nrsf_id, TRUE, val, CLR_MAX);
    if (!pick_color_menu(&color, buf))
        return FALSE;

    menu_num_breadcrumbs(buf, nrs_id, nrsf_id, TRUE, val, color);
    if (!pick_attr_menu(&attr_bits, buf))
        return FALSE;

    ca->color = color;
    ca->attr_bits = attr_bits;
    return TRUE;
}

/* Prompt for color and attributes for a new str_rule.  If neither menu is
 * cancelled, the color and attributes are stored in ca and TRUE is returned.
 * If either menu is cancelled, ca remains unaltered and FALSE is returned
 * instead.
 */
STATIC_OVL boolean
prompt_str_color_attr(ca, str)
struct status_color_attr *ca;
const char *str;
{
    int color;
    int attr_bits;
    char buf[BUFSZ];

    menu_str_breadcrumbs(buf, str, CLR_MAX);
    if (!pick_color_menu(&color, buf))
        return FALSE;

    menu_str_breadcrumbs(buf, str, color);
    if (!pick_attr_menu(&attr_bits, buf))
        return FALSE;

    ca->color = color;
    ca->attr_bits = attr_bits;
    return TRUE;
}

/* Prompt for the value, color and attributes of a new num_rule.  The ruleset
 * (e.g. HP) and ruleset field (e.g. (%) percent) should have already been
 * chosen at this point.  If all prompts are confirmed and the value is valid,
 * then the rule is added to the ruleset field, which essentially adds it to
 * the set of active rules.  If any prompt is cancelled or the value isn't
 * valid then rule addition is cancelled.
 */
STATIC_OVL void
prompt_add_num_rule(nrs_id, nrsf_id)
enum num_ruleset_id nrs_id;
enum num_ruleset_field_id nrsf_id;
{
    char qbuf[QBUFSZ];
    char buf[BUFSZ];
    long val, minval;
    struct status_color_attr ca;

    /* Get the value. */
    menu_num_breadcrumbs(qbuf, nrs_id, nrsf_id, FALSE, 0, CLR_MAX);
    Sprintf(eos(qbuf), " Enter %s:", num_ruleset_fields[nrsf_id].prompt);
    getlin(qbuf, buf);
    if (!buf[0] || buf[0] == '\033')
        return;
    if (!status_num_from_str(buf, &val, nrs_id, nrsf_id)) {
        pline("Invalid value \"%s\".", buf);
        return;
    }
    minval = num_ruleset_fields[nrsf_id].min_value;
    if (minval && val < minval) {
        pline("Invalid value %ld; below minimum (%ld).", val, minval);
        return;
    }

    /* Get the color and attributes. */
    if (!prompt_num_color_attr(&ca, val, nrs_id, nrsf_id))
        return;

    add_num_rule(nrs_id, nrsf_id, val, &ca, TRUE);
}

/* Prompt for the color and attributes of a new str_rule.  The value should
 * have already been chosen at this point (e.g. "Hungry").  If all prompts are
 * confirmed, then the str_rule becomes active and adopts the chosen color and
 * attributes.  If any prompt is cancelled, the str_rule is left untouched.
 */
STATIC_OVL void
prompt_add_str_rule(rules, rule_size, str)
struct str_rule *rules;
int rule_size;
const char *str;
{
    struct status_color_attr ca;
    if (!prompt_str_color_attr(&ca, str))
        return;
    add_str_rule(rules, rule_size, str, &ca, TRUE);
}

/* e.g. "Remove HP%100:green? [yn] (n)" */
STATIC_OVL void
prompt_remove_num_rule(nrs_id, nrsf_id, pos)
enum num_ruleset_id nrs_id;
enum num_ruleset_field_id nrsf_id;
int pos;
{
    struct num_rule *rule;
    int i;
    char qbuf[QBUFSZ];

    rule = *get_nrs_field(nrs_id, nrsf_id);
    i = pos;
    while (i > 0 && rule) {
        rule = rule->next;
        i--;
    }
    if (!rule)
        return;

    Strcpy(qbuf, "Remove ");
    str_from_num_rule(eos(qbuf), nrs_id, nrsf_id, rule);
    Strcat(qbuf, "?");
    if (yn(qbuf) == 'y')
        remove_num_rule(nrs_id, nrsf_id, pos);
}

/* e.g. "Remove Hungry:yellow? [yn] (n)" */
STATIC_OVL void
prompt_remove_str_rule(rules, rule_size, str)
struct str_rule *rules;
int rule_size;
const char *str;
{
    int i;
    char qbuf[QBUFSZ];

    for (i = 0; i < rule_size; i++) {
        if (!strcmpi(str, rules[i].val)) {
            break;
        }
    }
    if (i >= rule_size)
        return;

    Strcpy(qbuf, "Remove ");
    str_from_str_rule(eos(qbuf), &rules[i]);
    Strcat(qbuf, "?");
    if (yn(qbuf) == 'y')
        remove_str_rule(rules, rule_size, str);
}

/* Show a menu to add or remove rules from a ruleset field (e.g. HP), e.g.
 *
 * [HP | (%) percent] Add or remove percent rule:
 *
 * a - Add new percent rule
 * b - HP%0:red&blink&inverse
 * c - HP%15:red&inverse
 * d - HP%33:red&bold
 * e - HP%50:orange
 * f - HP%66:yellow
 * g - HP%100:green
 */
STATIC_OVL void
menu_num_ruleset_field(nrs_id, nrsf_id)
enum num_ruleset_id nrs_id;
enum num_ruleset_field_id nrsf_id;
{
    winid menu;
    int num_picks;

    do {
        anything any;
        menu_item *picks;
        struct num_rule *curr;
        char buf[BUFSZ];
        int i;

        menu = create_nhwindow(NHW_MENU);
        start_menu(menu);

        any.a_int = -1;
        Sprintf(buf, "Add new %s rule", num_ruleset_fields[nrsf_id].label);
        add_menu(menu, NO_GLYPH, &any, 0, 0, ATR_NONE, buf, MENU_UNSELECTED);

        curr = *get_nrs_field(nrs_id, nrsf_id);
        i = 1;
        while (curr) {
            any.a_int = i;
            str_from_num_rule(buf, nrs_id, nrsf_id, curr);
            add_menu(menu, NO_GLYPH, &any, 0, 0, ATR_NONE, buf,
                     MENU_UNSELECTED);

            curr = curr->next;
            i++;
        }

        menu_num_breadcrumbs(buf, nrs_id, nrsf_id, FALSE, 0, CLR_MAX);
        Sprintf(eos(buf), " Add or remove %s rule:",
                num_ruleset_fields[nrsf_id].label);
        end_menu(menu, buf);

        /* Flush screen so players can see their changes in real-time. */
        context.botlx = 1;
        flush_screen(0);
        num_picks = select_menu(menu, PICK_ONE, &picks);
        if (num_picks > 0) {
            if (picks[0].item.a_int == -1) {
                prompt_add_num_rule(nrs_id, nrsf_id);
            } else {
                prompt_remove_num_rule(nrs_id, nrsf_id,
                                       picks[0].item.a_int - 1);
            }
            free(picks);
        }
        destroy_nhwindow(menu);
    } while (num_picks > 0);
}

/* Show a menu to select a ruleset field (e.g. percent) of a ruleset (e.g. HP).
 *
 * e.g. [HP] Select rule type:
 *
 * a - (+) up
 * b - (-) down
 * c - (.) equal
 * d - (<) less than - (1 rule set)
 * e - (%) percent - (5 rules set)
 * f - (>) greater than
 */
STATIC_OVL void
menu_num_ruleset(nrs_id)
enum num_ruleset_id nrs_id;
{
    winid menu;
    int num_picks;

    do {
        anything any;
        menu_item *picks;
        char buf[BUFSZ];
        int i;

        menu = create_nhwindow(NHW_MENU);
        start_menu(menu);

        for (i = 0; i < MAX_NRSF; i++) {
            int num_rules;

            if (i == NRSF_PERCENT && !status_rulesets[nrs_id].has_max)
                continue;

            any.a_int = i + 1;
            Sprintf(buf, "(%c) %s", num_ruleset_fields[i].op,
                    num_ruleset_fields[i].label);
            num_rules = count_num_rules(*get_nrs_field(nrs_id, i));
            if (num_rules > 0) {
                Sprintf(eos(buf), " - (%d rule%s set)", num_rules,
                        num_rules == 1 ? "" : "s");
            }
            add_menu(menu, NO_GLYPH, &any, num_ruleset_fields[i].accel, 0,
                     ATR_NONE, buf, MENU_UNSELECTED);
        }

        menu_num_breadcrumbs(buf, nrs_id, MAX_NRSF, FALSE, 0, CLR_MAX);
        Strcat(buf, " Select rule type:");
        end_menu(menu, buf);

        num_picks = select_menu(menu, PICK_ONE, &picks);
        if (num_picks > 0) {
            i = picks[0].item.a_int - 1;
            menu_num_ruleset_field(nrs_id, i);
            free(picks);
        }
        destroy_nhwindow(menu);
    } while (num_picks > 0);
}

/* Show a menu of condition str_rules to add or remove, e.g.
 *
 * [Conditions] Select rule to modify:
 *
 * a - Satiated:orange
 * b - Hungry:yellow
 * c - Weak:red&bold
 * d - Fainting:red&inverse
 * e - Fainted:red&blink&inverse
 * f - Starved
 * ...
 * (etc.)
 *
 * This should be called with SRM_NONE for a top-level menu, which will show
 * options for sub-menus that will be handled by calling this function
 * recursively with the sub-menu's ID.
 */
STATIC_OVL void
menu_str_rules(srm_id)
enum str_rule_menu_id srm_id;
{
    winid menu;
    int num_picks;
    struct str_rule *rules = status_color_rules.conds;
    int rule_size = MAX_COND_RULES;

    do {
        anything any;
        menu_item *picks;
        char buf[BUFSZ];
        int i;

        menu = create_nhwindow(NHW_MENU);
        start_menu(menu);

        /* Top-level menu should show options for sub-menus. */
        if (srm_id == SRM_NONE) {
            int j;
            for (j = 1; j < MAX_SRM; j++) {
                int submenu_count = count_str_rules(rules, rule_size, j);
                /* Negative IDs to prevent clashes with top-level conditions. */
                any.a_int = -j;
                Strcpy(buf, str_rule_menu_headings[j]);
                if (submenu_count > 0) {
                    Sprintf(eos(buf), " - (%d rule%s set)", submenu_count,
                            submenu_count == 1 ? "" : "s");
                }
                add_menu(menu, NO_GLYPH, &any, 0, 0, ATR_NONE, buf,
                         MENU_UNSELECTED);
            }
        }

        for (i = 0; i < rule_size; i++) {
            const struct str_rule *rule = &rules[i];

            if (!rule->val || !rule->val[0])
                continue;

            if (rule->menu != srm_id)
                continue;

            any.a_int = i + 1;
            if (rule->active)
                str_from_str_rule(buf, rule);
            else
                Strcpy(buf, rule->val);
            add_menu(menu, NO_GLYPH, &any, 0, 0, ATR_NONE, buf,
                     MENU_UNSELECTED);
        }

        menu_str_breadcrumbs(buf, str_rule_menu_headings[srm_id], CLR_MAX);
        Strcat(buf, " Select rule to modify:");
        end_menu(menu, buf);

        /* Flush screen so players can see their changes in real-time. */
        context.botlx = 1;
        flush_screen(0);
        num_picks = select_menu(menu, PICK_ONE, &picks);
        if (num_picks > 0) {
            i = picks[0].item.a_int;
            if (i < 0) {
                /* Handle selection of sub-menus from top-level menu. */
                menu_str_rules(-i);
            } else {
                /* Condition indices were offset by one when added, so undo
                 * that here. */
                i--;
                if (rules[i].active)
                    prompt_remove_str_rule(rules, rule_size, rules[i].val);
                else
                    prompt_add_str_rule(rules, rule_size, rules[i].val);
            }
            free(picks);
        }
        destroy_nhwindow(menu);
    } while (num_picks > 0);
}

/* Show a menu allowing in-game customization of status color rules. */
void
status_colors_menu()
{
    winid menu;
    int num_picks;

    do {
        anything any;
        menu_item *picks;
        char buf[BUFSZ];
        int i, num_rules;

        menu = create_nhwindow(NHW_MENU);
        start_menu(menu);

        any.a_int = -1;
        add_menu(menu, NO_GLYPH, &any, 0, 0, ATR_NONE, "Display raw rules",
                 MENU_UNSELECTED);

        for (i = 0; i < MAX_NRS; i++) {
            any.a_int = i + 1;
            Strcpy(buf, status_rulesets[i].label);
            num_rules = count_num_ruleset(i);
            if (num_rules > 0) {
                Sprintf(eos(buf), " - (%d rule%s set)", num_rules,
                        num_rules == 1 ? "" : "s");
            }
            add_menu(menu, NO_GLYPH, &any, 0, 0, ATR_NONE, buf,
                     MENU_UNSELECTED);
        }

        any.a_int = -2;
        Strcpy(buf, "Conditions");
        num_rules = count_str_rules(status_color_rules.conds, MAX_COND_RULES,
                                    SRM_ANY);
        if (num_rules > 0) {
            Sprintf(eos(buf), " - (%d rule%s set)", num_rules,
                    num_rules == 1 ? "" : "s");
        }
        add_menu(menu, NO_GLYPH, &any, 0, 0, ATR_NONE, buf, MENU_UNSELECTED);

        Sprintf(buf, "Select field for statuscolors: (statuscolors are %s)",
                iflags.use_status_colors ? "ON" : "OFF");
        end_menu(menu, buf);

        /* Flush screen so players can see the status area while in the menu. */
        flush_screen(0);
        num_picks = select_menu(menu, PICK_ONE, &picks);

        if (num_picks > 0) {
            switch (picks[0].item.a_int) {
            case -1:
                display_raw_rules();
                break;
            case -2:
                menu_str_rules(SRM_NONE);
                break;
            default:
                i = picks[0].item.a_int - 1;
                menu_num_ruleset(i);
                break;
            }
            free(picks);
        }
        destroy_nhwindow(menu);
    } while (num_picks > 0);
}

/********************************************
 * Status population and color application. *
 ********************************************/

/* Determine the color and attributes a status field should adopt based on the
 * current status color rules.
 */
STATIC_OVL void
apply_num_ruleset(nrs_id, val, prev, maxval, ca)
enum num_ruleset_id nrs_id;
long val, prev, maxval;
struct status_color_attr *ca;
{
    int i;

    ca->color = NO_COLOR;
    ca->attr_bits = 0;

    for (i = 0; i < MAX_NRSF; i++) {
        const struct num_rule *curr;
        for (curr = *get_nrs_field(nrs_id, i); curr; curr = curr->next) {
            boolean cmp;
            switch (i) {
            case NRSF_UP:
                cmp = (val >= prev + curr->val);
                break;
            case NRSF_DOWN:
                cmp = (val <= prev - curr->val);
                break;
            case NRSF_EQUAL:
                cmp = (val == curr->val);
                break;
            case NRSF_LESS_THAN:
                cmp = (val < curr->val);
                break;
            case NRSF_PERCENT:
                cmp = (val * 100 <= maxval * curr->val);
                break;
            case NRSF_GREATER_THAN:
                cmp = (val > curr->val);
                break;
            case MAX_NRSF:
                /* Warning suppression. */
                cmp = FALSE;
                break;
            }
            if (cmp) {
                *ca = curr->ca;
                if (!iflags.use_color)
                    ca->color = NO_COLOR;
                return;
            }
        }
    }
}

/* Apply the current status color rules to determine the colors and attributes
 * of all status fields and conditions.
 */
STATIC_OVL void
apply_status_colors(curr_si, prev_si, sic)
const struct status_info *curr_si, *prev_si;
struct status_info_colors *sic;
{
    int i, j;

    for (i = 0; i < MAX_NRS; i++) {
        /* Strength is special-cased below. */
        if (i == NRS_ST)
            continue;
        apply_num_ruleset(i, get_status_info_field(curr_si, i),
                          get_status_info_field(prev_si, i),
                          get_status_info_field_max(curr_si, i),
                          get_status_color_attr(sic, i));
    }

    /* Special case for strength since it's made up of two parts. */
    apply_num_ruleset(NRS_ST,
                      strength_from_parts(curr_si->st, curr_si->st_extra),
                      strength_from_parts(prev_si->st, prev_si->st_extra),
                      -1, &sic->st);

    for (i = 0; i < MAX_STATUS_CONDS; i++) {
        sic->conds[i].color = NO_COLOR;
        sic->conds[i].attr_bits = 0;

        if (!curr_si->conds[i][0])
            continue;

        for (j = 0; j < MAX_COND_RULES; j++) {
            const struct str_rule *rule = &status_color_rules.conds[j];
            if (!rule->active)
                continue;
            if (!strcmpi(curr_si->conds[i], rule->val)) {
                sic->conds[i].color = iflags.use_color ? rule->ca.color
                                                       : NO_COLOR;
                sic->conds[i].attr_bits = rule->ca.attr_bits;
            }
        }
    }
}

/* The status system maintains a copy of the previous status info in order to
 * calculate which status fields have gone up or down.  This is done using a
 * page-flipping strategy, and this function flips the pages.
 */
void
flip_status_info()
{
    /* Neither status info page has been initialized yet. */
    if (which_status == -1)
        return;

    /* Flips between 0 and 1. */
    which_status = 1 - which_status;
}

#endif /* STATUS_COLORS */

STATIC_OVL void
add_status_info_cond(si, str)
struct status_info *si;
const char *str;
{
    int i = 0;
    while (i < SIZE(si->conds) && si->conds[i][0])
        i++;
    if (i >= SIZE(si->conds))
        panic("add_status_info_cond: condition limit exceeded (%d)", i);
    strncpy(si->conds[i], str, sizeof(si->conds[0]));
    si->conds[i][sizeof(si->conds[0]) - 1] = '\0';
}

/* Fill in all basic status info to be delivered to the window port. */
STATIC_OVL void
populate_status_info(si)
struct status_info *si;
{
    char buf[BUFSZ];
    int i, cap;

    /*
     *  Player name and title.
     */
    strncpy(si->name, plname, sizeof(si->name));
    si->name[sizeof(si->name) - 1] = '\0';
    si->name[0] = highc(si->name[0]);

    if (Upolyd) {
        strncpy(si->title, mons[u.umonnum].mname, sizeof(si->title));
        si->title[sizeof(si->title) - 1] = '\0';
        for (i = 0; si->title[i]; i++)
            if (i == 0 || si->title[i - 1] == ' ')
                si->title[i] = highc(si->title[i]);
    } else {
        strncpy(si->title, rank(), sizeof(si->title));
        si->title[sizeof(si->title) - 1] = '\0';
    }

    /* Strength */
    if (ACURR(A_STR) > STR18(100)) {
        si->st = ACURR(A_STR) - 100;
        si->st_extra = 0;
    } else if (ACURR(A_STR) > 18 && ACURR(A_STR) <= STR18(100)) {
        si->st = 18;
        si->st_extra = ACURR(A_STR) - 18;
    } else {
        si->st = ACURR(A_STR);
        si->st_extra = 0;
    }

    /*  Dexterity, constitution, intelligence, wisdom, charisma. */
    si->dx = ACURR(A_DEX);
    si->co = ACURR(A_CON);
    si->in = ACURR(A_INT);
    si->wi = ACURR(A_WIS);
    si->ch = ACURR(A_CHA);

    /* Alignment */
    if (u.ualign.type == A_CHAOTIC) {
        Strcpy(si->align, "Chaotic");
    } else if (u.ualign.type == A_NEUTRAL) {
        Strcpy(si->align, "Neutral");
    } else {
        Strcpy(si->align, "Lawful");
    }

    /* Score */
#ifdef SCORE_ON_BOTL
    si->score = botl_score();
#else
    si->score = 0;
#endif

    /*  Dungeon level. */
    describe_level(buf);
    mungspaces(buf);
    strncpy(si->dlvl, buf, sizeof(si->dlvl));
    si->dlvl[sizeof(si->dlvl) - 1] = '\0';

    /* Gold */
    strncpy(si->gold_sym, encglyph(objnum_to_glyph(GOLD_PIECE)),
            sizeof(si->gold_sym));
    si->gold_sym[sizeof(si->gold_sym) - 1] = '\0';
    si->gold = money_cnt(invent);

    /*  Hit points  */
    if (Upolyd) {
        si->hp = u.mh;
        si->hp_max = u.mhmax;
    } else {
        si->hp = u.uhp;
        si->hp_max = u.uhpmax;
    }

    /* Power (magical energy) */
    si->pw = u.uen;
    si->pw_max = u.uenmax;

    /* Armor class */
    si->ac = u.uac;

    /* Experience */
    if (Upolyd) {
        Strcpy(si->exp_label, "HD");
        si->exp_level = mons[u.umonnum].mlevel;
        si->exp_points = 0;
    } else {
        Strcpy(si->exp_label, "Xp");
        si->exp_level = u.ulevel;
        si->exp_points = u.uexp;
    }

    /* Time (moves) */
    si->turns = moves;

    /* Conditions */
    memset(si->conds, 0, sizeof(si->conds));
    if (hu_stat[u.uhs][0] && hu_stat[u.uhs][0] != ' ') {
        Strcpy(buf, hu_stat[u.uhs]);
        mungspaces(buf);
        add_status_info_cond(si, buf);
    }
    if (Confusion)
        add_status_info_cond(si, "Conf");
    if (Sick && (u.usick_type & SICK_VOMITABLE))
        add_status_info_cond(si, "FoodPois");
    if (Sick && (u.usick_type & SICK_NONVOMITABLE))
        add_status_info_cond(si, "Ill");
    if (Blind)
        add_status_info_cond(si, "Blind");
    if (Stunned)
        add_status_info_cond(si, "Stun");
    if (Hallucination)
        add_status_info_cond(si, "Hallu");
    if (Slimed)
        add_status_info_cond(si, "Slime");
    cap = near_capacity();
    if (cap > UNENCUMBERED)
        add_status_info_cond(si, enc_stat[cap]);

#ifdef SCORE_ON_BOTL
    si->show_score = flags.showscore;
#else
    si->show_score = FALSE;
#endif
    si->show_exp_points = !Upolyd && flags.showexp;
    si->show_turns = flags.time;
}

/* Update the status area with fresh status info.  This is called by the core
 * engine whenever the status area needs to be (re)displayed.  This gathers all
 * the status info, determines the colors and attributes that fields should be
 * rendered with (assuming compile-time and runtime options enable this), and
 * send it all to the window port.
 */
void
bot()
{
#ifdef STATUS_COLORS
    static struct status_info si[2];
#else
    static struct status_info si;
#endif
    struct status_info_colors sic = {
        { NO_COLOR, 0 },
        { NO_COLOR, 0 },
        { NO_COLOR, 0 },
        { NO_COLOR, 0 },
        { NO_COLOR, 0 },
        { NO_COLOR, 0 },
        { NO_COLOR, 0 },

        { NO_COLOR, 0 },
        { NO_COLOR, 0 },
        { NO_COLOR, 0 },
        { NO_COLOR, 0 },
        { NO_COLOR, 0 },
        { NO_COLOR, 0 },
        { NO_COLOR, 0 },

        {
            { NO_COLOR, 0 },
            { NO_COLOR, 0 },
            { NO_COLOR, 0 },
            { NO_COLOR, 0 },
            { NO_COLOR, 0 },
            { NO_COLOR, 0 },
            { NO_COLOR, 0 },
            { NO_COLOR, 0 },
            { NO_COLOR, 0 }
        }
    };

    if (!blinit)
        panic("bot before init.");
    if (!youmonst.data) {
        context.botl = context.botlx = 0;
        return;
    }

#ifdef STATUS_COLORS
    if (which_status == -1) {
        /* Populate both structs. */
        populate_status_info(&si[0]);
        si[1] = si[0];
        /* Kick off page flipping. */
        which_status = 0;
    } else {
        populate_status_info(&si[which_status]);
    }

    if (iflags.use_status_colors)
        apply_status_colors(&si[which_status], &si[1 - which_status], &sic);

    status_update(&si[which_status], &sic);
#else
    populate_status_info(&si);
    status_update(&si, &sic);
#endif

    context.botl = context.botlx = 0;
}

void
status_initialize()
{
    (*windowprocs.win_status_init)();
    blinit = TRUE;
}

void
status_finish()
{
#ifdef STATUS_COLORS
    free_rules();
#endif

    /* call the window port cleanup routine */
    (*windowprocs.win_status_finish)();
}

#endif /*STATUS_VIA_WINDOWPORT*/

/*botl.c*/
