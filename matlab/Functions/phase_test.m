function [full_gaze_data, variableNames, calculated] = test_phase(eyetracker, window, monitorWindow, pAudioHandle, condition)
%familiarization
%   test process of Generalization2 study

% % PRE-TEST: 
% % attention getter Duration?
% % 2 objects (car and dinosaur) pop up in the screen
% % baby explores both objects : we register 2 sec of looking time + 1 or 2 sec of looking away for each object (so we’re sure that both objects have been looked at) --> this is gaze-contingent, we dont move forward with trial until baby looks at both
% % flash + sound (“ping”) in the middle of the objects to re-direct baby in the centre ---> this is also gaze contingent, we need baby to look at the centre (at the flash) and stop looking elsewhere
% % TEST:
% % play the test word (which words are indicated  below)
% % if baby looks at correct object for 2 sec or more ---> objects moves and sounds for 2 sec, then stays up in the screen for 2 more sec = total duration 6 sec; 
% % if baby looks at incorrect, nothing happens and after 2 or 3 sec trial ends (to give baby time to realize that choice was incorrect)
% % let’s not randomize right/left location of images within condition otherwise the correct object will always be on the right (and babies may pick up on this!); we’ll randomize locations between conditions




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
global SCREEN_FRAME_RATE;
global Y_KEY;
global N_KEY;
global E_KEY;

TEST_STIM_DURATION = 12;
ATTENTION_GETTER_MAX_DURATION = 10;
ATTENTION_GETTER_DURATION = 2;
LOOKING_ATTENTION_GETTER = 1*SCREEN_FRAME_RATE;
LOOKING_OBJECT_PRE = 2;
LOOKING_OBJECT_MAX_DURATION = 30;
LOOKING_OBJECT_PRE_TEST = 2*SCREEN_FRAME_RATE;

LOOKING_OBJECT_TEST_MAX_DURATION_1 = 10;
LOOKING_OBJECT_TEST_MAX_DURATION_2 = 10;
LOOKING_OBJECT_TEST_MAX_DURATION_4 = 10;
LOOKING_OBJECT_TEST_1 = 2*SCREEN_FRAME_RATE;
LOOKING_OBJECT_TEST_2 = 2*SCREEN_FRAME_RATE;
LOOKING_OBJECT_TEST_3 = 2*SCREEN_FRAME_RATE;
NO_LOOKING_OBJECT = 2*SCREEN_FRAME_RATE;
LOOKING_INCORRECT_OBJECT = 0.5*SCREEN_FRAME_RATE;



variableNames={'SystemTimeStamp','meanX','meanY','meanValidity','lX','lY','lV','lPupil','lPupilV','rX','rY','rV','rPupil','rPupilV','process', 'extra'};
[screenXpixels, screenYpixels] = Screen('WindowSize', window);

%% Select & prepare Stim pairs Image(1) Sound(2)
IMAGE_R_INDEX = 1;
IMAGE_L_INDEX = 2;
SOUND_INDEX = 3;
IMAGE_TARGET = 4;

stim_list1 = {"green_dino.png" "yellow_car.png"   "kegike.wav" "R"; ...
             "green_dino.png" "yellow_car.png" "kekegi.wav" "L"; ...
            "green_dino.png" "yellow_car.png" "gikegi.wav" "R"; ...
             "green_dino.png" "yellow_car.png" "gigike.wav" "L"};

stim_list2 = {"yellow_car.png" "green_dino.png"  "kegike.wav" "R"; ...
            "yellow_car.png" "green_dino.png" "kekegi.wav" "L"; ...
             "yellow_car.png" "green_dino.png" "gikegi.wav" "R"; ...
            "yellow_car.png" "green_dino.png" "gigike.wav" "L"};

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
    imgfileName = strcat(STIM_PATH, string(stim_list(stimIndex, IMAGE_R_INDEX)));
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

audioFileName = strcat(STIM_PATH, "attention_getter.wav");
[waveData, ~] = audioread(audioFileName);
ATTENTION_GETTER.soundData = [waveData waveData]';




[centerX, centerY] = RectCenter(Screen('Rect', window));
calculated = sprintf('Type;L;R;Sound;Target;look;correct(s);incorrect(s)\n');


%empty the eyeTracker buffer
eyetracker.get_gaze_data();
full_gaze_data = [];
for trialIndex = 1:size(stim_list,1)
    %w2 = (TEXTURES(textureIndex).iRect(3)-TEXTURES(textureIndex).iRect(1))/2;
    %h2 = (TEXTURES(textureIndex).iRect(4)-TEXTURES(textureIndex).iRect(2))/2;
    %AOI = [screen_center(1)-w2 screen_center(2)-h2 screen_center(1)+w2 screen_center(2)+h2];
    
    
    %%% PRE-TEST
    %% attention_getter
    ATTENTION_GETTER.centerRect = CenterRectOnPoint(ATTENTION_GETTER.iRect, centerX, centerY);
    tIni = tic;
    ATTENTION_GETTER.looking_count = 0;
    while toc(tIni)<ATTENTION_GETTER_DURATION
        Screen('AsyncFlipEnd', monitorWindow);
        ATTENTION_GETTER.centerRect = CenterRectOnPoint(ATTENTION_GETTER.iRect*ATTENTION_GETTER.zoom, centerX, centerY);
        Screen('DrawTexture', window, ATTENTION_GETTER.texture, ATTENTION_GETTER.iRect, ATTENTION_GETTER.centerRect, ATTENTION_GETTER.rotation);
        Screen('DrawTexture', monitorWindow, ATTENTION_GETTER.texture, ATTENTION_GETTER.iRect, ATTENTION_GETTER.centerRect/SCREEN_MONITOR_PROPORTION, ATTENTION_GETTER.rotation);
        ATTENTION_GETTER.rotation = ATTENTION_GETTER.rotation + STEEP_ROTATION;
        ATTENTION_GETTER.zoom = 0.5+sin(ATTENTION_GETTER.rotation/100)/4;
        gaze_data = eyetracker.get_gaze_data();
        full_gaze_data = [full_gaze_data; parseGazeData(gaze_data, ["test_pre", strcat(ATTENTION_GETTER.name)])];
        if ~isempty(gaze_data)
            [x, y] = lastGazeData(gaze_data);
            x = x*screenXpixels;
            y = y*screenYpixels;
            if DRAW_CUES
                Screen('FrameRect', window, [0 255 0], ATTENTION_GETTER.centerRect, 2);
            end
            Screen('FrameRect', monitorWindow, [0 255 0], ATTENTION_GETTER.centerRect/SCREEN_MONITOR_PROPORTION, 2);
            if DRAW_CUES
                Screen('DrawDots', window, [x y], 80, EYE_DOT_COLOR, [], 2);
            end
            Screen('DrawDots', monitorWindow, [x y]/SCREEN_MONITOR_PROPORTION, 80, EYE_DOT_COLOR, [], 2);
            if inrect([x y], ATTENTION_GETTER.centerRect)
                %animation
                if DRAW_CUES
                    Screen('FrameRect', window, [255 0 0], ATTENTION_GETTER.centerRect, 5);
                end
                Screen('FrameRect', monitorWindow, [255 0 0], ATTENTION_GETTER.centerRect/SCREEN_MONITOR_PROPORTION, 5);
                
                ATTENTION_GETTER.looking_count = ATTENTION_GETTER.looking_count + 1;
            end
            %if (ATTENTION_GETTER.looking_count>ATTENTION_GETTER_DURATION)
            %    break;
            %end
        end
        Screen('AsyncFlipBegin', monitorWindow);
        Screen('Flip', window);
        [ keyIsDown, ~, keyCode ] = KbCheck;
        if find(keyCode, 1)== E_KEY
            throw(MException('Generalization:warning', 'End by experimenter: inside test'))
        end
    end
    
    % 2 objects (car and dinosaur) pop up in the screen
    % baby explores both objects : we register 2 sec of looking time + 1 or 2 sec of looking away for each object 
    %  (so we’re sure that both objects have been looked at) --> this is gaze-contingent, we dont move forward with 
    %  trial until baby looks at both
    tIni=tic;
    steep_rotation=STEEP_ROTATION;
    while toc(tIni)<LOOKING_OBJECT_MAX_DURATION
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
        full_gaze_data = [full_gaze_data; parseGazeData(gaze_data, ["test_pre_1", strcat(TEXTURES_L(trialIndex).name, ",", TEXTURES_R(trialIndex).name, ",", WAVEDATA(trialIndex).name)])];
        if ~isempty(gaze_data)
            [x, y] = lastGazeData(gaze_data);
            x = x*screenXpixels;
            y = y*screenYpixels;
            if inrect([x y], TEXTURES_L(trialIndex).AOI)
                TEXTURES_L(trialIndex).looking_count = TEXTURES_L(trialIndex).looking_count + 1;
                if DRAW_CUES
                    Screen('FrameRect', window, [255 0 0], TEXTURES_L(trialIndex).AOI, 5);
                end
                Screen('FrameRect', monitorWindow, [255 0 0], TEXTURES_L(trialIndex).AOI/SCREEN_MONITOR_PROPORTION, 5);
            end
            if inrect([x y], TEXTURES_R(trialIndex).AOI)
                TEXTURES_R(trialIndex).looking_count = TEXTURES_R(trialIndex).looking_count + 1;
                if DRAW_CUES
                    Screen('FrameRect', window, [255 0 0], TEXTURES_R(trialIndex).AOI, 5);
                end
                Screen('FrameRect', monitorWindow, [255 0 0], TEXTURES_R(trialIndex).AOI/SCREEN_MONITOR_PROPORTION, 5);
            end
            
            if DRAW_CUES
                Screen('DrawDots', window, [x y], 80, EYE_DOT_COLOR, [], 2);
                [nx, ny, textbounds, wordbounds] = DrawFormattedText(window, sprintf('L: %i/%i, R: %i/%i', TEXTURES_L(trialIndex).looking_count, LOOKING_OBJECT_PRE_TEST, TEXTURES_R(trialIndex).looking_count, LOOKING_OBJECT_PRE_TEST), 10, 100, [0 0 0]);
            end
            Screen('DrawDots', monitorWindow, [x y]/SCREEN_MONITOR_PROPORTION, 80, EYE_DOT_COLOR, [], 2);
            [nx, ny, textbounds, wordbounds] = DrawFormattedText(monitorWindow, sprintf('L: %i/%i, R: %i/%i', TEXTURES_L(trialIndex).looking_count, LOOKING_OBJECT_PRE_TEST, TEXTURES_R(trialIndex).looking_count, LOOKING_OBJECT_PRE_TEST), 10, 100, [0 0 0]);
            [ keyIsDown, ~, keyCode ] = KbCheck;
            if find(keyCode, 1) == E_KEY
                throw(MException('Generalization:warning', 'End by experimenter: inside test'))
            end
        end
        Screen('AsyncFlipBegin', monitorWindow);
        Screen('Flip', window);

        if (TEXTURES_L(trialIndex).looking_count>LOOKING_OBJECT_PRE_TEST && TEXTURES_R(trialIndex).looking_count>LOOKING_OBJECT_PRE_TEST)
            break;
        end
    end
    
    if ~(TEXTURES_L(trialIndex).looking_count>LOOKING_OBJECT_PRE_TEST && TEXTURES_R(trialIndex).looking_count>LOOKING_OBJECT_PRE_TEST)
        continue;
    end
    
    % % flash + sound (“ping”) in the middle of the objects to re-direct baby in the centre ---> 
    % % this is also gaze contingent, we need baby to look at the centre (at the flash) and stop looking elsewhere
    
    PsychPortAudio('FillBuffer', pAudioHandle, ATTENTION_GETTER.soundData);
    volume = PsychPortAudio('Volume', pAudioHandle, 1.0);
% %     PsychPortAudio('Start', pAudioHandle, 0, 0, 1); % repeat sound "for ever"
    PsychPortAudio('Start', pAudioHandle);
    
    ATTENTION_GETTER.centerRect = CenterRectOnPoint(ATTENTION_GETTER.iRect, centerX, centerY);
    tIni = tic;
    ATTENTION_GETTER.looking_count = 0;
    while toc(tIni)<ATTENTION_GETTER_MAX_DURATION
        status = PsychPortAudio('GetStatus', pAudioHandle);
        if ~status.Active
            PsychPortAudio('Start', pAudioHandle);
        end
        Screen('AsyncFlipEnd', monitorWindow);
        Screen('DrawTexture', window, TEXTURES_L(trialIndex).texture, TEXTURES_L(trialIndex).iRect, TEXTURES_L(trialIndex).AOI, TEXTURES_L(trialIndex).rotation);
        Screen('DrawTexture', window, TEXTURES_R(trialIndex).texture, TEXTURES_R(trialIndex).iRect, TEXTURES_R(trialIndex).AOI, TEXTURES_R(trialIndex).rotation);
        Screen('AsyncFlipEnd', monitorWindow);
        Screen('DrawTexture', monitorWindow, TEXTURES_L(trialIndex).texture, TEXTURES_L(trialIndex).iRect, TEXTURES_L(trialIndex).AOI/SCREEN_MONITOR_PROPORTION, TEXTURES_L(trialIndex).rotation);
        Screen('DrawTexture', monitorWindow, TEXTURES_R(trialIndex).texture, TEXTURES_R(trialIndex).iRect, TEXTURES_R(trialIndex).AOI/SCREEN_MONITOR_PROPORTION, TEXTURES_R(trialIndex).rotation);
        
        ATTENTION_GETTER.centerRect = CenterRectOnPoint(ATTENTION_GETTER.iRect*ATTENTION_GETTER.zoom, centerX, centerY);
        Screen('DrawTexture', window, ATTENTION_GETTER.texture, ATTENTION_GETTER.iRect, ATTENTION_GETTER.centerRect, ATTENTION_GETTER.rotation);
        Screen('DrawTexture', monitorWindow, ATTENTION_GETTER.texture, ATTENTION_GETTER.iRect, ATTENTION_GETTER.centerRect/SCREEN_MONITOR_PROPORTION, ATTENTION_GETTER.rotation);
        ATTENTION_GETTER.rotation = ATTENTION_GETTER.rotation + STEEP_ROTATION;
        ATTENTION_GETTER.zoom = 0.5+sin(ATTENTION_GETTER.rotation/100)/4;
        gaze_data = eyetracker.get_gaze_data();
        full_gaze_data = [full_gaze_data; parseGazeData(gaze_data, ["test_pre", strcat(ATTENTION_GETTER.name)])];
        if ~isempty(gaze_data)
            [x, y] = lastGazeData(gaze_data);
            x = x*screenXpixels;
            y = y*screenYpixels;
            if DRAW_CUES
                Screen('FrameRect', window, [0 255 0], ATTENTION_GETTER.centerRect, 2);
                Screen('DrawDots', window, [x y], 80, EYE_DOT_COLOR, [], 2);
                DrawFormattedText(monitorWindow, sprintf('A.GETTER: %i', ATTENTION_GETTER.looking_count), 10, 100, [0 0 0]);
            end
            Screen('FrameRect', monitorWindow, [0 255 0], ATTENTION_GETTER.centerRect/SCREEN_MONITOR_PROPORTION, 2);
            Screen('DrawDots', monitorWindow, [x y]/SCREEN_MONITOR_PROPORTION, 80, EYE_DOT_COLOR, [], 2);
            DrawFormattedText(monitorWindow, sprintf('L: %i, R: %i', ATTENTION_GETTER.looking_count), 10, 100, [0 0 0]);
            if inrect([x y], ATTENTION_GETTER.centerRect)
                %animation
                if DRAW_CUES
                    Screen('FrameRect', window, [255 0 0], ATTENTION_GETTER.centerRect, 5);
                end
                Screen('FrameRect', monitorWindow, [255 0 0], ATTENTION_GETTER.centerRect/SCREEN_MONITOR_PROPORTION, 5);
                
                ATTENTION_GETTER.looking_count = ATTENTION_GETTER.looking_count + 1;
            end
            if (ATTENTION_GETTER.looking_count>LOOKING_ATTENTION_GETTER)
                break;
            end
        end
        Screen('AsyncFlipBegin', monitorWindow);
        Screen('Flip', window);
        [ keyIsDown, ~, keyCode ] = KbCheck;
        if find(keyCode, 1)== E_KEY
            throw(MException('Generalization:warning', 'End by experimenter: inside test'))
        end
    end
    status = PsychPortAudio('GetStatus', pAudioHandle);
    while status.Active
        status = PsychPortAudio('GetStatus', pAudioHandle);
        pause(0.1);
    end
    %%PsychPortAudio('Stop', pAudioHandle);
    
    
    
    %% TEST
    % % play the test word (which words are indicated  below)
    % % if baby looks at correct object for 2 sec or more ---> objects moves and sounds for 2 sec, 
    % %   then stays up in the screen for 2 more sec = total duration 6 sec; 
    % % if baby looks at incorrect, nothing happens and after 2 or 3 sec trial ends (to give baby time to realize that choice was incorrect)
    
    
    sec=1/Screen('NominalFrameRate', window);
    TEXTURES_L(trialIndex).rotation = 0;
    TEXTURES_R(trialIndex).rotation = 0;
    TEXTURES_L(trialIndex).looking_count = 0;
    TEXTURES_R(trialIndex).looking_count = 0;
    correct=0;
    incorrect=0;
    look='OUT';
     
    if (string(stim_list(trialIndex, IMAGE_TARGET))=="L")
        AOI_correct = TEXTURES_L(trialIndex).AOI;
        AOI_incorrect = TEXTURES_R(trialIndex).AOI;
    else
        AOI_correct = TEXTURES_R(trialIndex).AOI;
        AOI_incorrect = TEXTURES_L(trialIndex).AOI;
    end
    
    PsychPortAudio('FillBuffer', pAudioHandle, WAVEDATA(trialIndex).data);
% %     volume = PsychPortAudio('Volume', pAudioHandle, 1.0);
% %     PsychPortAudio('Start', pAudioHandle, 0, 0, 1); % repeat sound "for ever"
    status = PsychPortAudio('GetStatus', pAudioHandle);
    if ~status.Active
        PsychPortAudio('Start', pAudioHandle);
    end
    
    tIni=tic;
    while toc(tIni)<LOOKING_OBJECT_TEST_MAX_DURATION_1
        look='OUT';
        status = PsychPortAudio('GetStatus', pAudioHandle);
        if ~status.Active
            PsychPortAudio('Start', pAudioHandle);
        end
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
        full_gaze_data = [full_gaze_data; parseGazeData(gaze_data, ["test", strcat(TEXTURES_L(trialIndex).name, ",", TEXTURES_R(trialIndex).name, ",", WAVEDATA(trialIndex).name,",",string(stim_list(trialIndex, IMAGE_TARGET)))])];
        if ~isempty(gaze_data)
            [x, y] = lastGazeData(gaze_data);
            x = x*screenXpixels;
            y = y*screenYpixels;
            if inrect([x y], AOI_correct)
                look="OK";
                correct = correct+1;
                if (string(stim_list(trialIndex, IMAGE_TARGET))=="L")
                    TEXTURES_L(trialIndex).looking_count = TEXTURES_L(trialIndex).looking_count + 1;
                else
                    TEXTURES_R(trialIndex).looking_count = TEXTURES_R(trialIndex).looking_count + 1;
                end
            end
            % % if baby looks at incorrect, nothing happens and after 2 or 3 (LOOKING_INCORRECT_OBJECT) sec trial ends (to give baby time to realize that choice was incorrect)
            if inrect([x y], AOI_incorrect)
                look="NOK";
                incorrect = incorrect+1;
                if (string(stim_list(trialIndex, IMAGE_TARGET))=="L")
                    TEXTURES_R(trialIndex).looking_count = TEXTURES_R(trialIndex).looking_count + 1;
                    if TEXTURES_R(trialIndex).looking_count>LOOKING_INCORRECT_OBJECT
                        break;
                    end
                else
                    TEXTURES_L(trialIndex).looking_count = TEXTURES_L(trialIndex).looking_count + 1;
                    if TEXTURES_L(trialIndex).looking_count>LOOKING_INCORRECT_OBJECT
                        break;
                    end
                end
            end
            if DRAW_CUES
                Screen('DrawDots', window, [x y], 80, EYE_DOT_COLOR, [], 2);
                DrawFormattedText(window, sprintf('L: %i, R: %i, Target:%s', TEXTURES_L(trialIndex).looking_count, TEXTURES_R(trialIndex).looking_count, string(stim_list(trialIndex, IMAGE_TARGET))), 10, 100, [0 0 0]);
            end
            Screen('DrawDots', monitorWindow, [x y]/SCREEN_MONITOR_PROPORTION, 80, EYE_DOT_COLOR, [], 2);
            DrawFormattedText(monitorWindow, sprintf('L: %i, R: %i, Target:%s', TEXTURES_L(trialIndex).looking_count, TEXTURES_R(trialIndex).looking_count, string(stim_list(trialIndex, IMAGE_TARGET))), 10, 100, [0 0 0]);
            [ keyIsDown, ~, keyCode ] = KbCheck;
            if find(keyCode, 1) == E_KEY
                throw(MException('Generalization:warning', 'End by experimenter: inside test'))
            end
        
            if ((string(stim_list(trialIndex, IMAGE_TARGET))=="L") && (TEXTURES_L(trialIndex).looking_count>LOOKING_OBJECT_TEST_2))
                break;
            elseif ((string(stim_list(trialIndex, IMAGE_TARGET))=="R") && (TEXTURES_R(trialIndex).looking_count>LOOKING_OBJECT_TEST_2))
                break;
            end
            tmpStr = sprintf('PRE-TEST;%s;%s;%s;%s;%s;%.2f;%.2f\n', TEXTURES_L(trialIndex).name, TEXTURES_R(trialIndex).name, WAVEDATA(trialIndex).name, string(stim_list(trialIndex, IMAGE_TARGET)), look, correct*sec, incorrect*sec );
            calculated = [calculated, tmpStr];
            Screen('AsyncFlipBegin', monitorWindow);
            Screen('Flip', window);
        end
        
    end
    


    TEXTURES_L(trialIndex).rotation = 0;
    TEXTURES_R(trialIndex).rotation = 0;
    TEXTURES_L(trialIndex).looking_count = 0;
    TEXTURES_R(trialIndex).looking_count = 0;
    correct=0;
    incorrect=0;
    look='OUT';
    
    firstLook = true;
    
    tIni=tic;
    steep_rotation=STEEP_ROTATION;
    while toc(tIni)<LOOKING_OBJECT_TEST_MAX_DURATION_2
        look='OUT';
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
            if inrect([x y], AOI_correct)
                look='OK';
                correct=correct+1;
                firstLook = false;
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
                status = PsychPortAudio('GetStatus', pAudioHandle);
                if ~status.Active
                    PsychPortAudio('Start', pAudioHandle);
                end
                % audio volume UP
% %                 if volume<1.0
% %                     if volume>0.8
% %                         volume = 1;
% %                         PsychPortAudio('Start', pAudioHandle, 0, 0, 1);
% %                     else
% %                         volume = volume+STEEP_SOUND_LEVEL_UP;
% %                     end
% %                 end
            else
                if inrect([x y], AOI_incorrect)
                    look='NOK';
                    incorrect=incorrect+1;
                    if (string(stim_list(trialIndex, IMAGE_TARGET))=="L")
                        TEXTURES_R(trialIndex).looking_count = TEXTURES_R(trialIndex).looking_count + 1;
                        if TEXTURES_R(trialIndex).looking_count>LOOKING_INCORRECT_OBJECT
                            break;
                        end
                    else
                        TEXTURES_L(trialIndex).looking_count = TEXTURES_L(trialIndex).looking_count + 1;
                        if TEXTURES_L(trialIndex).looking_count>LOOKING_INCORRECT_OBJECT
                            break;
                        end
                    end
                end
                
                
                
                % audio volume DOWN
                if ~firstLook 
%                     if volume>0
%                         if volume<0.2
%                             volume = 0;
%                             PsychPortAudio('Stop', pAudioHandle);
%                         else
%                             volume = volume-STEEP_SOUND_LEVEL_DOWN;
%                         end
%                     end
                end
            end
%             PsychPortAudio('Volume', pAudioHandle , volume);
            if DRAW_CUES
                Screen('DrawDots', window, [x y], 80, EYE_DOT_COLOR, [], 2);
            end
            Screen('DrawDots', monitorWindow, [x y]/SCREEN_MONITOR_PROPORTION, 80, EYE_DOT_COLOR, [], 2);
            [ keyIsDown, ~, keyCode ] = KbCheck;
            if find(keyCode, 1) == E_KEY
                throw(MException('Generalization:warning', 'End by experimenter: inside test'))
            end
        end
        
        
        
        tmpStr = sprintf('TEST;%s;%s;%s;%s;%s;%.2f;%.2f\n', TEXTURES_L(trialIndex).name, TEXTURES_R(trialIndex).name, WAVEDATA(trialIndex).name, string(stim_list(trialIndex, IMAGE_TARGET)), look, correct*sec, incorrect*sec );
        calculated = [calculated, tmpStr];
        
        Screen('AsyncFlipBegin', monitorWindow);
        Screen('Flip', window);
    end
    status = PsychPortAudio('GetStatus', pAudioHandle);
    while status.Active
        status = PsychPortAudio('GetStatus', pAudioHandle);
        pause(0.1);
    end
% %     while volume>0
% %         volume = PsychPortAudio('Volume', pAudioHandle , volume-STEEP_SOUND_LEVEL_DOWN);
% %         pause(0.01);
% %     end
    
end

% % while volume>0
% %     volume = PsychPortAudio('Volume', pAudioHandle , volume-STEEP_SOUND_LEVEL_DOWN);
% %     pause(0.01);
% % end

eyetracker.stop_gaze_data();
PsychPortAudio('Stop', pAudioHandle);
Screen('AsyncFlipEnd', monitorWindow);



end