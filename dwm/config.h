/* See LICENSE file for copyright and license details. */
/* build and install with makepkg -efi */

/* dan: this version is really good!!! */

/* for audio */
#include <X11/XF86keysym.h>

/* appearance */
   // TO USE TERMINUS, you might gotta rehash it, see ~/.xinitrc
// static const char font[]            = "-*-inconsolata-medium-r-*-*-14-*-*-*-*-*-*-*";
// static const char font[]            = "-*-inconsolata-*-*-*-*-*-*-*-*-*-*-*-*";
// static const char font[]            = "-*-terminus-medium-r-*-*-14-*-*-*-*-*-*-*";
static const char font[]            = "-*-terminus-medium-r-*-*-12-*-*-*-*-*-*-*";
// static const char font[]            = "-*-clean-medium-*-*-*-12-*-*-*-*-70-*-*";
// static const char font[]            = "-*-*-medium-*-*-*-14-*-*-*-*-*-*-*";

/* kinda cute fonts
// static const char font[]            = "-*-urw chancery l-*-*-*-*-*-*-*-*-*-*-*-*"; // so loopy
// static const char font[]            = "-*-urw bookman l-medium-r-*-*-*-*-*-*-*-*-*-*"; // really big though
// static const char font[]            = "-*-urw gothic l-*-r-*-*-*-*-*-*-*-*-*-*"; // really big though
*/ // end of kinda cute

// static const char normbordercolor[] = "#444444";
static const char normbordercolor[] = "#000000";
static const char normbgcolor[]     = "#222222";
static const char normfgcolor[]     = "#bbbbbb";
// static const char selbordercolor[]  = "#005577";
static const char selbordercolor[]  = "#E80572"; // magenta
// static const char selbgcolor[]      = "#005577"; // default blue
// static const char selbgcolor[]      = "#570000"; // maroon
// static const char selbgcolor[]      = "#4A003F"; // deep purple
static const char selbgcolor[]      = "#004675"; // blue
static const char selfgcolor[]      = "#eeeeee";
static const unsigned int borderpx  = 1;        /* border pixel of windows */
static const unsigned int snap      = 32;       /* snap pixel */
static const Bool showbar           = True;     /* False means no bar */
static const Bool topbar            = True;     /* False means bottom bar */

/* tagging */
static const char *tags[] = {"0", "1", "2", "3", "4", "5", "6", "7", "8", "9" };
// static const char *tags[] = { "1", "2", "3", "4", "5", "6", "7", "8", "9" };

static const Rule rules[] = {
	/* class      instance    title       tags mask     isfloating   monitor */
	{ "Gimp",     NULL,       NULL,       0,            True,        -1 },
	{ "Firefox",  NULL,       NULL,       1 << 9,       False,       -1 },
// 	{ "Firefox",  NULL,       NULL,       1 << 8,       False,       -1 },
// 	{ "Firefox",  NULL,       "Firefox Preferences",1 << 8,True,     -1 },
	{ NULL,       NULL,  "Firefox Preferences",1 << 8,  True,        -1 },
	{ "opera",    NULL,       NULL,       1 << 7,       False,       -1 },
	{ "Tilda",    NULL,       NULL,       0,            True,        -1 },
	{ "Thunderbird", NULL,    NULL,       1 << 8,       False,       -1 },
	{ "Hexchat",  NULL,       NULL,       1 << 7,       False,       -1 },
	{ NULL,       NULL,       "WeeChat",  1 << 7,       False,       -1 },
	{ "Chromium", NULL,       NULL,       1 << 6,       False,       -1 },
	{ "ibreoffice",NULL,      NULL,       1,            False,       -1 },
	{ "Zenity",   NULL,       NULL,       ~0,           False,       -1 },
	{ "feh",      NULL,       NULL,       0,            True,        -1 },
};

/* layout(s) */
// static const float mfact      = 0.55; /* factor of master area size [0.05..0.95] */
static const float mfact      = 0.5513; /* 79 chars wide for urxvt with inconsolata */
static const int nmaster      = 1;    /* number of clients in master area */
static const Bool resizehints = False; /* True means respect size hints in tiled resizals */

static const Layout layouts[] = {
	/* symbol     arrange function */
	{ "[]=",      tile },    /* first entry is default */
	{ "><>",      NULL },    /* no layout function means floating behavior */
	{ "[M]",      monocle },
};

/* key definitions */
#define MODKEY Mod4Mask
#define TAGKEYS(KEY,TAG) \
	{ MODKEY,                       KEY,      view,           {.ui = 1 << TAG} }, \
	{ MODKEY|ControlMask,           KEY,      toggleview,     {.ui = 1 << TAG} }, \
	{ MODKEY|ShiftMask,             KEY,      tag,            {.ui = 1 << TAG} }, \
	{ MODKEY|ControlMask|ShiftMask, KEY,      toggletag,      {.ui = 1 << TAG} },

/* helper for spawning shell commands in the pre dwm-5.0 fashion */
// #define SHCMD(cmd) { .v = (const char*[]){ "/bin/sh", "-c", cmd, NULL } }
#define SHCMD(cmd) { .v = (const char*[]){ "/bin/dash", "-c", cmd, NULL } }

/* commands */
// static const char *dmenucmd[] = { "dmenu_run", "-fn", font, "-nb", normbgcolor, "-nf", normfgcolor, "-sb", selbgcolor, "-sf", selfgcolor, NULL };
static const char *olddmenucmd[] = { "dmenu_run", "-fn", font, "-nb", normbgcolor, "-nf", normfgcolor, "-sb", selbgcolor, "-sf", selfgcolor, NULL };
static const char *dmenucmd[] = { "dmenu_launcher", "-fn", font, "-nb", normbgcolor, "-nf", normfgcolor, "-sb", selbgcolor, "-sf", selfgcolor, NULL };
// static const char *termcmd[]  = { "uxterm", NULL };
// static const char *termcmd[]  = { "urxvt", NULL };
static const char *termcmd[]  = { "urxvtc", NULL };

/* custom commands */
/* alsamixer, volume control */
static const char *upvol[] = { "amixer", "set", "Master", "3+", "-q", NULL};
static const char *downvol[] = { "amixer", "set", "Master", "3-", "-q", NULL};
static const char *mutevol[] = { "amixer", "set", "Master", "toggle", "-q", NULL};

/* mpc controls */
static const char *mpctoggle[] = { "mpc", "toggle", "--quiet", NULL};
static const char *mpcstop[] = { "mpc", "stop", "--quiet", NULL};
static const char *mpcprev[] = { "mpc", "prev", "--quiet", NULL};
static const char *mpcnext[] = { "mpc", "next", "--quiet", NULL};
static const char *songinfo[] = { "songinfo", NULL};
static const char *statuscmd[] = { "dwmstatus", "--update", NULL};

/* other */
// static const char *whine[] = { "zenity", "--warning", NULL};
static const char *browser[] = { "firefox", NULL};
static const char *chromium[] = { "chromium", NULL};
// static const char *mail[] = { "thunderbird", NULL};
static const char *wallpaper[] = { "wallpaper-updater", NULL};

/* middle click */
static const char *middleclick[] = { "xdotool", "click", "2", NULL}; 

/* enable or disable the touchpad */
static const char *touchpadoff[] = { "xinput", "--disable", "DLL064D:00 06CB:2985", NULL}; 
// static const char *touchpadoff[] = { "xinput", "set-int-prop", "13", "Device Enabled", "8", "0", NULL}; 
// xinput set-int-prop 13 "Device Enabled" 8 0
static const char *touchpadon[] = { "xinput", "--enable", "DLL064D:00 06CB:2985", NULL}; 
// static const char *touchpadon[] = { "xinput", "set-int-prop", "13", "Device Enabled", "8", "1", NULL};
// xinput set-int-prop 13 "Device Enabled" 8 1

static Key keys[] = {
	/* modifier                     key        function        argument */
	{ MODKEY,                       XK_p,      spawn,          {.v = dmenucmd } },
	{ MODKEY|ShiftMask,             XK_p,      spawn,          {.v = olddmenucmd } },
	{ MODKEY|ShiftMask,             XK_Return, spawn,          {.v = termcmd } },
	{ MODKEY,                       XK_b,      togglebar,      {0} },
	{ MODKEY,                       XK_j,      focusstack,     {.i = +1 } },
	{ MODKEY,                       XK_k,      focusstack,     {.i = -1 } },
	{ MODKEY,                       XK_i,      incnmaster,     {.i = +1 } },
	{ MODKEY,                       XK_d,      incnmaster,     {.i = -1 } },
// 	{ MODKEY,                       XK_h,      setmfact,       {.f = -0.05} },
	{ MODKEY,                       XK_h,      setmfact,       {.f = -0.03} },
// 	{ MODKEY,                       XK_l,      setmfact,       {.f = +0.05} },
	{ MODKEY,                       XK_l,      setmfact,       {.f = +0.03} },
	{ MODKEY,                       XK_Return, zoom,           {0} },
	{ MODKEY,                       XK_Tab,    view,           {0} },
	{ MODKEY|ShiftMask,             XK_c,      killclient,     {0} },
	{ MODKEY,                       XK_t,      setlayout,      {.v = &layouts[0]} },
	{ MODKEY,                       XK_f,      setlayout,      {.v = &layouts[1]} },
	{ MODKEY,                       XK_m,      setlayout,      {.v = &layouts[2]} },
	{ MODKEY,                       XK_space,  setlayout,      {0} },
	{ MODKEY|ShiftMask,             XK_space,  togglefloating, {0} },
// 	{ MODKEY,                       XK_0,      view,           {.ui = ~0 } },
	{ MODKEY|ShiftMask,             XK_0,      tag,            {.ui = ~0 } },
	{ MODKEY,                       XK_comma,  focusmon,       {.i = -1 } },
	{ MODKEY,                       XK_period, focusmon,       {.i = +1 } },
	{ MODKEY|ShiftMask,             XK_comma,  tagmon,         {.i = -1 } },
	{ MODKEY|ShiftMask,             XK_period, tagmon,         {.i = +1 } },
	TAGKEYS(                        XK_0,                      0)
	TAGKEYS(                        XK_1,                      1)
	TAGKEYS(                        XK_2,                      2)
	TAGKEYS(                        XK_3,                      3)
	TAGKEYS(                        XK_4,                      4)
	TAGKEYS(                        XK_5,                      5)
	TAGKEYS(                        XK_6,                      6)
	TAGKEYS(                        XK_7,                      7)
	TAGKEYS(                        XK_8,                      8)
	TAGKEYS(                        XK_9,                      9)
	{ MODKEY|ShiftMask,             XK_q,      quit,           {0} },

    /* custom */
    { 0,              XF86XK_AudioRaiseVolume, spawn,          {.v = upvol} },
    { 0,              XF86XK_AudioPlay,        spawn,          {.v = mpctoggle} },
    { MODKEY,         XF86XK_AudioPlay,        spawn,          {.v = songinfo} },
    { 0,              XF86XK_AudioStop,        spawn,          {.v = mpcstop} },
    { 0,              XF86XK_AudioPrev,        spawn,          {.v = mpcprev} },
    { 0,              XF86XK_AudioNext,        spawn,          {.v = mpcnext} },
    // i guess there is no audiopause button on my keyboard
//     { 0,              XF86XK_AudioPause,       spawn,          {.v = whine} },
    { 0,              XF86XK_AudioLowerVolume, spawn,          {.v = downvol} },
    { 0,              XF86XK_AudioMute,        spawn,          {.v = mutevol} },
    // { MODKEY,          XF86XK_WWW,              spawn,          {.v = browser} },
    { 0,              XF86XK_HomePage,         spawn,          {.v = browser} },
    { MODKEY,         XF86XK_HomePage,         spawn,          {.v = chromium} },
//     { MODKEY,         XK_a,                    spawn,          {.v = browser} },
//     { MODKEY,         XK_w,                    spawn,          {.v = wallpaper} },
    { MODKEY,         XK_w,                    spawn,          SHCMD("$HOME/bin/wallpaper-updater") },
    { MODKEY|ShiftMask,XK_w,                   spawn,          {.v = statuscmd} },
    // touchpad
    { MODKEY,         XK_z,                    spawn,          {.v = touchpadoff} },
    { MODKEY|ShiftMask,XK_z,                   spawn,          {.v = touchpadon}  },
    // my tab key is getting worn out! :(
	{ MODKEY,         XK_apostrophe,            view,           {0} },
// 	{ MODKEY,         XK_F1,                   view,           {0} },
// 	{ MODKEY,         XK_semicolon,            view,           {0} },
};

/* button definitions */
/* click can be ClkLtSymbol, ClkStatusText, ClkWinTitle, ClkClientWin, or ClkRootWin */
static Button buttons[] = {
	/* click                event mask      button          function        argument */
	{ ClkLtSymbol,          0,              Button1,        setlayout,      {0} },
	{ ClkLtSymbol,          0,              Button3,        setlayout,      {.v = &layouts[2]} },
	{ ClkWinTitle,          0,              Button2,        zoom,           {0} },
	{ ClkStatusText,        0,              Button2,        spawn,          {.v = termcmd } },
	{ ClkStatusText,        0,              Button1,        spawn,          {.v = statuscmd } },
	{ ClkRootWin,           0,              Button1,        spawn,          {.v = termcmd } },
	{ ClkRootWin,           0,              Button3,        spawn,          {.v = wallpaper } },
	{ ClkClientWin,         MODKEY,         Button1,        movemouse,      {0} },
	{ ClkClientWin,         MODKEY,         Button2,        togglefloating, {0} },
	{ ClkClientWin,         MODKEY,         Button3,        resizemouse,    {0} },
	{ ClkTagBar,            0,              Button1,        view,           {0} },
	{ ClkTagBar,            0,              Button3,        toggleview,     {0} },
	{ ClkTagBar,            MODKEY,         Button1,        tag,            {0} },
	{ ClkTagBar,            MODKEY,         Button3,        toggletag,      {0} },
};
