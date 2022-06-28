function [eyeTackerWindow, monitorWindow] = graphicsInitialize()
%UNTITLED Summary of this function goes here
%   Detailed explanation goes here
global SCREEN_MONITOR_PROPORTION;
global BACKGROUND_COLOR;
global SCREEN_FRAME_RATE;
global SCREEN_WIDTH;
global SCREEN_HEIGHT;


disp('[INI] Initialize Graphics');

Screen('Preference', 'SkipSyncTests', 1);
%Screen('Preference', 'SkipSyncTests', 0);

screens = Screen('Screens');

% So in a situation where we
% have two screens attached to our monitor we will draw to the external
% screen. When only one screen is attached to the monitor we will draw to
% this. For help see: help max
screenNumber = max(screens);
EYETRACKER_SCREEN = 1;
MONITOR_SCREEN = 2;

Screen('Resolution', EYETRACKER_SCREEN, SCREEN_WIDTH, SCREEN_HEIGHT);

% Open an on screen window and color it black.
% For help see: Screen OpenWindow?
[eyeTackerWindow, rectEyeTackerWindow] = Screen('OpenWindow', EYETRACKER_SCREEN, BACKGROUND_COLOR);
[monitorWindow, ~] = Screen('OpenWindow', MONITOR_SCREEN, BACKGROUND_COLOR, rectEyeTackerWindow/SCREEN_MONITOR_PROPORTION );



% Enable alpha blending for anti-aliasing
% For help see: Screen BlendFunction?
% Also see: Chapter 6 of the OpenGL programming guide
Screen('BlendFunction', eyeTackerWindow, 'GL_SRC_ALPHA', 'GL_ONE_MINUS_SRC_ALPHA');
Screen('BlendFunction', monitorWindow, 'GL_SRC_ALPHA', 'GL_ONE_MINUS_SRC_ALPHA');

Screen('TextSize', eyeTackerWindow, 30);
Screen('TextSize', monitorWindow, 30/SCREEN_MONITOR_PROPORTION);

disp('[END] Initialize Graphics');
end

