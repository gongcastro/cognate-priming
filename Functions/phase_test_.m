function [full_gaze_data, variableNames, calculated] = test_phase(eyetracker, window, monitorWindow, pAudioHandle, condition)
%familiarization
%   test process of Generalization2 study

global STIM_PATH;
global STEEP_SOUND_LEVEL_UP;
global STEEP_SOUND_LEVEL_DOWN;
global LIMIT_ROTATION;
global STEEP_ROTATION;
global TEXT_COLOR;
global BACKGROUND_COLOR;
global SCREEN_MONITOR_PROPORTION;
global EYE_DOT_COLOR;
global DRAW_CUES;   %for DEBUG allow to dray AOI rectangle ans gazePoint
global Y_KEY;
global N_KEY;
global E_KEY;

TEST_STIM_DURATION = 12;

variableNames={'SystemTimeStamp','meanX','meanY','meanValidity','lX','lY','lV','lPupil','lPupilV','rX','rY','rV','rPupil','rPupilV','process', 'extra'};
[screenXpixels, screenYpixels] = Screen('WindowSize', window);

%% Select & prepare Stim pairs Image(1) Sound(2)
IMAGE_L_INDEX = 1;
IMAGE_R_INDEX = 2;
SOUND_INDEX = 3;
IMAGE_TARGET = 4;

stim_list1 = {"green_dino.png" "red_car.png"   "kegike.wav" "R"; ...
            "yellow_car.png" "blue_dino.png" "kekegi.wav" "L"; ...
            "green_dino.png" "red_car.png" "gikegi.wav" "R"; ...
            "yellow_car.png" "green_dino.png" "gigike.wav" "L"};

stim_list2 = {"green_dino.png" "red_car.png"   "kekegi.wav" "R"; ...
            "yellow_car.png" "blue_dino.png" "kegike.wav" "L"; ...
            "green_dino.png" "red_car.png" "gigike.wav" "R"; ...
            "yellow_car.png" "green_dino.png" "gikegi.wav" "L"};

if condition==1
    stim_list=stim_list1;
else    
    stim_list=stim_list2;
end

% load stim images
% Import image and and convert it, stored in
% MATLAB matrix, into a Psychtoolbox OpenGL texture using 'MakeTexture';
for stimIndex = 1:size(stim_list,1)
    imgfileName = strcat(STIM_PATH, string(stim_list(stimIndex, IMAGE_L_INDEX)));
    [img, ~, alpha] = imread(imgfileName, 'png');
    % Crop image if it is larger then screen size. There's no image scaling
    % in maketexture
    [iy, ix, ~] = size(img);
    img(:, :, 4) = alpha;
    TEXTURES_L(stimIndex).name = imgfileName;
    TEXTURES_L(stimIndex).texture = Screen('MakeTexture', window, img);
    TEXTURES_L(stimIndex).iRect = [0 0 ix iy];
    TEXTURES_L(stimIndex).AOI = CenterRectOnPoint([0 0 ix iy], 450, 540);
    TEXTURES_L(stimIndex).looking_count = 0;
    TEXTURES_L(stimIndex).no_looking_count = 0;
    TEXTURES_L(stimIndex).rotation = 0;
   
end

for stimIndex = 1:size(stim_list,1)
    imgfileName = strcat(STIM_PATH, string(stim_list(stimIndex, IMAGE_R_INDEX)))
    [img, ~, alpha] = imread(imgfileName, 'png');
    % Crop image if it is larger then screen size. There's no image scaling
    % in maketexture
    [iy, ix, ~] = size(img);
    img(:, :, 4) = alpha;
    TEXTURES_R(stimIndex).name = imgfileName;
    TEXTURES_R(stimIndex).texture = Screen('MakeTexture', window, img);
    TEXTURES_R(stimIndex).iRect = [0 0 ix iy];
    TEXTURES_R(stimIndex).AOI = CenterRectOnPoint([0 0 ix iy], 1470, 540);
    TEXTURES_R(stimIndex).looking_count = 0;
    TEXTURES_R(stimIndex).no_looking_count = 0;
    TEXTURES_R(stimIndex).rotation = 0;
end

% load stim sound
for stimIndex = 1:size(stim_list,1)
    audioFileName = strcat(STIM_PATH, string(stim_list(stimIndex, SOUND_INDEX)));
    %%audioFileName = strcat(STIM_PATH, "duduba.wav");
    [waveData, ~] = audioread(audioFileName);
    WAVEDATA(stimIndex).data = [waveData waveData]';
    WAVEDATA(stimIndex).name = audioFileName;
end

%% Select & prepare attention getters
imgfileName = strcat(STIM_PATH, "attention_getter3.png");
[img, ~, alpha] = imread(imgfileName, 'png');
% Crop image if it is larger then screen size. There's no image scaling
% in maketexture
[iy, ix, ~] = size(img);
img(:, :, 4) = alpha;
ATTENTION_GETTER.name = imgfileName;
ATTENTION_GETTER.texture = Screen('MakeTexture', window, img);
ATTENTION_GETTER.iRect = [0 0 ix iy];
ATTENTION_GETTER.rotation = 0;
ATTENTION_GETTER.zoom = 1;
ATTENTION_GETTER.looking_count = 0;




[centerX, centerY] = RectCenter(Screen('Rect', window));


%empty the eyeTracker buffer
eyetracker.get_gaze_data();
full_gaze_data = [];
for trialIndex = 1:size(stim_list,1)
    %w2 = (TEXTURES(textureIndex).iRect(3)-TEXTURES(textureIndex).iRect(1))/2;
    %h2 = (TEXTURES(textureIndex).iRect(4)-TEXTURES(textureIndex).iRect(2))/2;
    %AOI = [screen_center(1)-w2 screen_center(2)-h2 screen_center(1)+w2 screen_center(2)+h2];
    if (string(stim_list(trialIndex, IMAGE_TARGET))=="L")
        AOI = TEXTURES_L(trialIndex).AOI;
    else
        AOI = TEXTURES_R(trialIndex).AOI;
    end
    TEXTURES_L(trialIndex).rotation = 0;
    TEXTURES_R(trialIndex).rotation = 0;
    
    
    PsychPortAudio('FillBuffer', pAudioHandle, WAVEDATA(stimIndex).data);
    PsychPortAudio('Start', pAudioHandle, 0, 0, 1); % repeat sound "for ever"
    volume = PsychPortAudio('Volume', pAudioHandle, 1.0);
    tIni=tic;
    steep_rotation=STEEP_ROTATION;
    while toc(tIni)<TEST_STIM_DURATION
        
        %Screen('DrawLine', window, [1 0 0], r(1), r(2), r(3), r(4)); 
        %Screen('DrawLine', window, [1 0 0], r(1), r(4), r(3), r(2));
        
        Screen('DrawTexture', window, TEXTURES_L(trialIndex).texture, TEXTURES_L(trialIndex).iRect, TEXTURES_L(trialIndex).AOI, TEXTURES_L(trialIndex).rotation);
        Screen('DrawTexture', window, TEXTURES_R(trialIndex).texture, TEXTURES_R(trialIndex).iRect, TEXTURES_R(trialIndex).AOI, TEXTURES_R(trialIndex).rotation);
        Screen('AsyncFlipEnd', monitorWindow);
        Screen('DrawTexture', monitorWindow, TEXTURES_L(trialIndex).texture, TEXTURES_L(trialIndex).iRect, TEXTURES_L(trialIndex).AOI/SCREEN_MONITOR_PROPORTION, TEXTURES_L(trialIndex).rotation);
        Screen('DrawTexture', monitorWindow, TEXTURES_R(trialIndex).texture, TEXTURES_R(trialIndex).iRect, TEXTURES_R(trialIndex).AOI/SCREEN_MONITOR_PROPORTION, TEXTURES_R(trialIndex).rotation);
        if DRAW_CUES
            Screen('FrameRect', window, [0 255 0], TEXTURES_L(trialIndex).AOI, 5);
            Screen('FrameRect', window, [0 255 0], TEXTURES_R(trialIndex).AOI, 5);
        end
        Screen('FrameRect', monitorWindow, [0 255 0], TEXTURES_L(trialIndex).AOI/SCREEN_MONITOR_PROPORTION, 5);
        Screen('FrameRect', monitorWindow, [0 255 0], TEXTURES_R(trialIndex).AOI/SCREEN_MONITOR_PROPORTION, 5);
        gaze_data = eyetracker.get_gaze_data();
        full_gaze_data = [full_gaze_data; parseGazeData(gaze_data, ["test", strcat(TEXTURES_L(trialIndex).name, ",", TEXTURES_R(trialIndex).name, ",", WAVEDATA(trialIndex).name)])];
        if ~isempty(gaze_data)
            [x, y] = lastGazeData(gaze_data);
            x = x*screenXpixels;
            y = y*screenYpixels;
            if inrect([x y], AOI)
                if (string(stim_list(trialIndex, IMAGE_TARGET))=="L")
                    TEXTURES_L(trialIndex).looking_count = TEXTURES_L(trialIndex).looking_count + 1;
                    if TEXTURES_L(trialIndex).rotation>LIMIT_ROTATION || TEXTURES_L(trialIndex).rotation<-LIMIT_ROTATION
                        steep_rotation=-steep_rotation; 
                    end
                    TEXTURES_L(trialIndex).rotation=TEXTURES_L(trialIndex).rotation+steep_rotation;
                else
                    TEXTURES_R(trialIndex).looking_count = TEXTURES_R(trialIndex).looking_count + 1;
                    if TEXTURES_R(trialIndex).rotation>LIMIT_ROTATION || TEXTURES_R(trialIndex).rotation<-LIMIT_ROTATION
                        steep_rotation=-steep_rotation; 
                    end
                    TEXTURES_R(trialIndex).rotation=TEXTURES_R(trialIndex).rotation+steep_rotation;
                end
                % audio volume UP
                if volume<1.0
                    if volume>0.8
                        volume = 1;
                    else
                        volume = volume+STEEP_SOUND_LEVEL_UP;
                    end
                end
            else
                % audio volume DOWN
                if volume>0
                    if volume<0.2
                        volume = 0;
                    else
                        volume = volume-STEEP_SOUND_LEVEL_DOWN;
                    end
                end
            end
            PsychPortAudio('Volume', pAudioHandle , volume);
            if DRAW_CUES
                Screen('DrawDots', window, [x y], 80, EYE_DOT_COLOR, [], 2);
            end
            Screen('DrawDots', monitorWindow, [x y]/SCREEN_MONITOR_PROPORTION, 80, EYE_DOT_COLOR, [], 2);
            [ keyIsDown, ~, keyCode ] = KbCheck;
            if find(keyCode, 1) == E_KEY
                throw(MException('Generalization:warning', 'End by experimenter: inside test'))
            end
        end
        
        Screen('AsyncFlipBegin', monitorWindow);
        Screen('Flip', window);
    end
    while volume>0
        volume = PsychPortAudio('Volume', pAudioHandle , volume-STEEP_SOUND_LEVEL_DOWN);
        pause(0.01);
    end
    PsychPortAudio('Stop', pAudioHandle);
end

while volume>0
    volume = PsychPortAudio('Volume', pAudioHandle , volume-STEEP_SOUND_LEVEL_DOWN);
    pause(0.01);
end

eyetracker.stop_gaze_data();
PsychPortAudio('Stop', pAudioHandle);
Screen('AsyncFlipEnd', monitorWindow);

sec=1/Screen('NominalFrameRate', window);

calculated = sprintf('ImageL,ImageR,Sound,Target,lookedL(s),lookedR(s)\n');
for i = 1:size(stim_list, 2)
    tmpStr = sprintf('%s,%s,%s,%s,%.2f,%.2f\n', TEXTURES_L(i).name, TEXTURES_R(i).name, WAVEDATA(i).name, string(stim_list(trialIndex, IMAGE_TARGET)), TEXTURES_L(i).looking_count*sec, TEXTURES_R(i).looking_count*sec );
    calculated = strcat(calculated, tmpStr);
end

end