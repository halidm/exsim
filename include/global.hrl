%%%%%%%%%%%%%%%%%%%%%%
%%%    GLOBAL	   %%%
%%%%%%%%%%%%%%%%%%%%%%


%% SYSTEM SETTINGS
-define(NODE, node()).
-define(APPLICATION, exsim).
-define(ENV(Key), application:get_env(?APPLICATION, Key, [])).
-define(ENV(Key, Default), application:get_env(?APPLICATION, Key, Default)).
-define(ENV(App, Key, Default), application:get_env(App, Key, Default)).


%% GENERAL SETTINGS
-define(TIME_SCALE, ?ENV(time_scale)).
-define(TIME_START, ?ENV(time_start)).
