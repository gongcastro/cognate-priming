function [full_gaze_data, variableNames, calculated] = pre_fam_phase(eyetracker, window, monitorWindow, pAudioHandle)
%% familiarization
%   familiarization process of Generalization2 study
%   Presentation of ALL images (2 dino, 2 cars) altogether with music for 20 secs
%   When look at an object, this, rotate a litle bit
%   Saves all eyetracker data: time, mean of L/R eyes gaze, validity, L/R eyes: gaze,
%   validity, L/R Pupil: size, validity
%   Saves calculation (aproximation): time looking every object

global STIM_PATH;
global PRE_FAM_DURATION;
global LIMIT_ROTATION;
global STEEP_ROTATION;
global EYE_DOT_COLOR;
global SCREEN_MONITOR_PROPORTION;
global DRAW_CUES;   %for DEBUG allow to dray AOI rectangle ans gazePoint
global E_KEY;
SPEED = -1;

variableNames={'SystemTimeStamp','meanX','meanY','meanValidity','lX','lY','lV','lPupil','lPupilV','rX','rY','rV','rPupil','rPupilV','process', 'extra'};
[screenXpixels, screenYpixels] = Screen('WindowSize', window);

image_file_list = ["blue_dino.png", "red_car.png", "green_dino.png", "yellow_car.png"];
sound = "pre_fam.wav";

%% generate oval movement path
MAX_POINTS = 800;
theta=linspace(0,2*pi,MAX_POINTS);
points = zeros(size(theta,2), 2);
centerX = 1920/2;
centerY = 1080/2;
radiusX=centerX-200;
radiusY=centerY-200;

for i=1: size(theta,2)
    points(i,1) = centerX+radiusX*cos(theta(i));
    points(i,2) = centerY+radiusY*sin(theta(i));
end

%% load stim images
% Import image and and convert it, stored in
% MATLAB matrix, into a Psychtoolbox OpenGL texture using 'MakeTexture';
for textureIndex = 1:size(image_file_list,2)
    myimgfile = strcat(STIM_PATH, image_file_list(textureIndex));
    [img, ~, alpha] = imread(myimgfile, 'png');
    % Crop image if it is larger then screen size. There's no image scaling
    % in maketexture
    [iy, ix, ~] = size(img);
    img(:, :, 4) = alpha;
    TEXTURES(textureIndex).name = image_file_list(textureIndex);
    TEXTURES(textureIndex).texture = Screen('MakeTexture', window, img);
    TEXTURES(textureIndex).iRect = [0 0 ix iy];
    TEXTURES(textureIndex).x = 0;
    TEXTURES(textureIndex).y = 0;
    TEXTURES(textureIndex).r = 0;
    TEXTURES(textureIndex).rSteep = STEEP_ROTATION;
    TEXTURES(textureIndex).looking = 0; % keeps the number of frames that look into
end

%% load stim sound
fileName = strcat(STIM_PATH, filesep, sound);
[waveData, ~] = audioread(fileName);
PsychPortAudio('FillBuffer', pAudioHandle, waveData');
PsychPortAudio('Start', pAudioHandle, 0, 0, 1); % repeat sound "for ever"

%%empty the eyeTracker buffer
eyetracker.get_gaze_data();
full_gaze_data = [];

%% main loop
tIni = tic;
index = 1;
AOIText = '';
% clear keyBuffer
while KbCheck
    KbReleaseWait;
end

while toc(tIni)<PRE_FAM_DURATION
    Screen('AsyncFlipEnd', monitorWindow);
    gaze_data = eyetracker.get_gaze_data();
    [x, y] = lastGazeData(gaze_data); %x,y are from 0..1
    x = x*screenXpixels; % scale to screen size
    y = y*screenYpixels;
    for textureIndex = 1:size(image_file_list, 2)
        TEXTURES(textureIndex).x =  points(mod(index+(MAX_POINTS/4)*textureIndex, MAX_POINTS)+1,1);
        TEXTURES(textureIndex).y =  points(mod(index+(MAX_POINTS/4)*textureIndex, MAX_POINTS)+1,2);
        AOI = CenterRectOnPoint(TEXTURES(textureIndex).iRect/2, TEXTURES(textureIndex).x, TEXTURES(textureIndex).y);
        if DRAW_CUES
            Screen('FrameRect', window, [0 255 0], AOI, 2);
        end
        lookText = 'LOOK_NONE';
        if inrect([x y], AOI)
            TEXTURES(textureIndex).looking = TEXTURES(textureIndex).looking+1;
            lookText = strcat('LOOK_', TEXTURES(textureIndex).name);
            if TEXTURES(textureIndex).r>LIMIT_ROTATION || TEXTURES(textureIndex).r<-LIMIT_ROTATION
                TEXTURES(textureIndex).rSteep=-TEXTURES(textureIndex).rSteep; 
            end
            TEXTURES(textureIndex).r=TEXTURES(textureIndex).r+TEXTURES(textureIndex).rSteep;
            if DRAW_CUES
                Screen('FrameRect', window, [255 0 0], AOI, 5);
            end
        end
        
        Screen('DrawTexture', window, TEXTURES(textureIndex).texture, TEXTURES(textureIndex).iRect, AOI, TEXTURES(textureIndex).r);
        Screen('DrawTexture', monitorWindow, TEXTURES(textureIndex).texture, TEXTURES(textureIndex).iRect, AOI/SCREEN_MONITOR_PROPORTION, TEXTURES(textureIndex).r);
    end
    
    full_gaze_data = [full_gaze_data; parseGazeData(gaze_data, ["pre_fam_phase", lookText])];

    if DRAW_CUES
        Screen('DrawDots', window, [x y], 80, EYE_DOT_COLOR, [], 2);
    end
    Screen('DrawDots', monitorWindow, [x y]/SCREEN_MONITOR_PROPORTION, 80, EYE_DOT_COLOR, [], 2);
    Screen('AsyncFlipBegin', monitorWindow);

    Screen('Flip', window);
    index=index+SPEED;
    
    [ keyIsDown, ~, keyCode ] = KbCheck;
    if find(keyCode, 1) == E_KEY
        throw(MException('Generalization:warning', 'End by experimenter: inside pre-familiarization'))
    end
end

eyetracker.stop_gaze_data();
PsychPortAudio('Stop', pAudioHandle);
Screen('AsyncFlipEnd', monitorWindow);

sec=1/Screen('NominalFrameRate', window);
calculated = '';
for textureIndex = 1:size(image_file_list, 2)
    tmpStr = sprintf('Image: %s\t\t looked for: %.2f seconds\r\n', TEXTURES(textureIndex).name, TEXTURES(textureIndex).looking*sec );
    calculated = [calculated, tmpStr];
end


end


