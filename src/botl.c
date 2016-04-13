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

STATIC_DCL void FDECL(add_status_info_cond,
                      (struct status_info *, const char *));
STATIC_DCL void FDECL(populate_status_info, (struct status_info *));

static boolean blinit = FALSE;

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

void
bot()
{
    struct status_info si;

    if (!blinit)
        panic("bot before init.");
    if (!youmonst.data) {
        context.botl = context.botlx = 0;
        return;
    }

    populate_status_info(&si);
    status_update(&si);

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
    /* call the window port cleanup routine */
    (*windowprocs.win_status_finish)();
}

#endif /*STATUS_VIA_WINDOWPORT*/

/*botl.c*/
