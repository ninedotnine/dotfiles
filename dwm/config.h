/* See LICENSE file for copyright and license details. */
/* build and install with makepkg -efi */

/* dan: this version is really good!!! */

/* for audio */
#include <X11/XF86keysym.h>

/* appearance */
static const char font[]            = "-*-terminus-medium-r-*-*-12-*-*-*-*-*-*-*";
static const char normbordercolor[] = "#444444"; // grey
static const char selbordercolor[]  = "#E80572"; // magenta
static const char normbgcolor[]     = "#10271e"; // deep green
static const char normfgcolor[]     = "#b779bc"; // light purple
static const char selbgcolor[]      = "#132d22"; // deep green
static const char selfgcolor[]      = "#b6b6b6"; // grey
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
	{ "Firefox",  NULL,       "Library",  0,            True,        -1 },
	{ "opera",    NULL,       NULL,       1 << 7,       False,       -1 },
	{ "Tilda",    NULL,       NULL,       ~0,           True,        -1 },
	{ "Thunderbird", NULL,    NULL,       1 << 8,       False,       -1 },
	{ "Hexchat",  NULL,       NULL,       1 << 7,       False,       -1 },
	{ NULL,       NULL,       "WeeChat",  1 << 7,       False,       -1 },
	{ "hromium",  NULL,       NULL,       1 << 6,       False,       -1 },
	{ "ibreoffice",NULL,      NULL,       1,            False,       -1 },
	{ "Zenity",   NULL,       NULL,       ~0,           False,       -1 },
	{ "feh",      NULL,       NULL,       0,            True,        -1 },
// 	{ "sxiv",     NULL,       NULL,       0,            True,        -1 },
	{ NULL,       NULL,       "sxiv",     0,            True,        -1 },
	{ NULL,       NULL,       "pinentry", 0,            True,        -1 },
	{ NULL,       NULL,       "MPlayer",  0,            True,        -1 },
	{ NULL,       NULL,       "Video Preview", ~0,      True,        -1 },
};

/* layout(s) */
// static const float mfact      = 0.55; /* factor of master area size [0.05..0.95] */
// static const float mfact      = 0.5513; /* 79 chars wide for urxvt with inconsolata */
static const float mfact      = 0.4913; /* 79 chars wide for urxvt with inconsolata */
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
static const char *olddmenucmd[] = { "dmenu_run", "-nb", normbgcolor, "-nf", normfgcolor, "-sb", selbgcolor, "-sf", selfgcolor, NULL };
static const char *dmenucmd[] = { "dmenu_launcher", "-nb", normbgcolor, "-nf", normfgcolor, "-sb", selbgcolor, "-sf", selfgcolor, NULL };
// static const char *termcmd[]  = { "uxterm", NULL };
// static const char *termcmd[]  = { "urxvt", NULL };
static const char *termcmd[]  = { "urxvtc", NULL };

/* custom commands */
/* alsamixer, volume control */
static const char *upvol[] = { "amixer", "set", "Master", "4%+", "-q", NULL};
static const char *downvol[] = { "amixer", "set", "Master", "4%-", "-q", NULL};
static const char *mutevol[] = { "amixer", "set", "Master", "toggle", "-q", NULL};

/* mpc controls */
static const char *mpctoggle[] = { "mpc", "toggle", "--quiet", NULL};
static const char *mpcstop[] = { "mpc", "stop", "--quiet", NULL};
static const char *mpcprev[] = { "mpc", "prev", "--quiet", NULL};
static const char *mpcnext[] = { "mpc", "next", "--quiet", NULL};
static const char *songinfo[] = { "songinfo", NULL};
static const char *statuscmd[] = { "dwmstatus", "--update", "--no-network", NULL};

/* screen brightness */
static const char *upbright[] = { "brighten", NULL};
static const char *downbright[] = { "brighten", "-d", NULL};

/* other */
static const char *whine[] = { "zenity", "--warning", NULL};
static const char *browser[] = { "firefox", NULL};
static const char *chromium[] = { "chromium", NULL};
// static const char *mail[] = { "thunderbird", NULL};
static const char *wallpaper[] = { "wallpaper-updater", NULL};

/* middle click */
static const char *middleclick[] = { "xdotool", "click", "2", NULL};

/* enable or disable the touchpad */
static const char *touchpadoff[] = { "xinput", "--disable", "Synaptics s3203", NULL};
static const char *touchpadon[] = { "xinput", "--enable", "Synaptics s3203", NULL};

/* shutdown and poweroff the computer */
static const char *shutoff[] = { "shutoff", NULL};


static Key keys[] = {
	/* modifier                     key        function        argument */
	{ MODKEY,                       XK_p,      spawn,          {.v = dmenucmd } },
	{ MODKEY|ShiftMask,             XK_p,      spawn,          {.v = olddmenucmd } },
	{ MODKEY|ShiftMask,             XK_Return, spawn,          {.v = termcmd } },
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
    /* control music */
    { 0,              XF86XK_AudioMedia,        spawn,          SHCMD("/usr/bin/mpd") },
    { 0,              XF86XK_AudioRaiseVolume, spawn,          {.v = upvol} },
    { 0,              XF86XK_AudioPlay,        spawn,          {.v = mpctoggle} },
    { MODKEY,         XF86XK_AudioPlay,        spawn,          {.v = songinfo} },
    { 0,              XF86XK_AudioStop,        spawn,          {.v = mpcstop} },
    { 0,              XF86XK_AudioPrev,        spawn,          {.v = mpcprev} },
    { 0,              XF86XK_AudioNext,        spawn,          {.v = mpcnext} },
    { MODKEY,         XK_Left,                 spawn,          {.v = mpcprev} },
    { MODKEY,         XK_Right,                spawn,          {.v = mpcnext} },
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
    // brightness

    { 0,              XF86XK_MonBrightnessUp,  spawn,          {.v = upbright} },
    { 0,              XF86XK_MonBrightnessDown,spawn,          {.v = downbright} },
    // touchpad
    { MODKEY,         XK_z,                    spawn,          {.v = touchpadoff} },
    { MODKEY|ShiftMask,XK_z,                   spawn,          {.v = touchpadon}  },
    // shutoff
    { MODKEY,ShiftMask,XK_s,                    spawn,          {.v = shutoff} },

    { MODKEY,         XK_x,                    spawn,          {.v = middleclick} },
    // my tab key is getting worn out! :(
	{ MODKEY,         XK_apostrophe,            view,           {0} },
// 	{ MODKEY,         XK_F1,                   view,           {0} },
// 	{ MODKEY,         XK_semicolon,            view,           {0} },
	{ MODKEY,         XK_semicolon,            spawn,          {.v = whine} },
};

/* button definitions */
/* click can be ClkLtSymbol, ClkStatusText, ClkWinTitle, ClkClientWin, or ClkRootWin */
static Button buttons[] = {
	/* click                event mask      button          function        argument */
	{ ClkLtSymbol,          0,              Button1,        setlayout,      {0} },
	{ ClkLtSymbol,          0,              Button3,        setlayout,      {.v = &layouts[2]} },
	{ ClkStatusText,        0,              Button1,        spawn,          {.v = statuscmd } },
	{ ClkRootWin,           0,              Button1,        spawn,          {.v = termcmd } },
	{ ClkRootWin,           0,              Button3,        spawn,          {.v = wallpaper } },
	{ ClkClientWin,         MODKEY,         Button1,        movemouse,      {0} },
	{ ClkClientWin,         MODKEY,         Button3,        resizemouse,    {0} },
	{ ClkTagBar,            0,              Button1,        view,           {0} },
	{ ClkTagBar,            0,              Button3,        toggleview,     {0} },
	{ ClkTagBar,            MODKEY,         Button1,        tag,            {0} },
	{ ClkTagBar,            MODKEY,         Button3,        toggletag,      {0} },
};
