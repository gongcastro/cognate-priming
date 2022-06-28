function [full_gaze_data, variableNames, calculated] = fam_phase(eyetracker, window, monitorWindow, pAudioHandle, condition)
%% Familiarization
%   familiarization process of Generalization2 study
%   Show an image-sound pair
%   If look at the image, rotate
%      

global STIM_PATH;
global RES_PATH;
global FAM_STIM_DURATION;
global SCREEN_FRAME_RATE;
global STEEP_SOUND_LEVEL_UP;
global STEEP_SOUND_LEVEL_DOWN;
global LIMIT_ROTATION;
global STEEP_ROTATION;
global TEXT_COLOR;
global BACKGROUND_COLOR;
global EYE_DOT_COLOR;
global SCREEN_MONITOR_PROPORTION;
global DRAW_CUES;   %for DEBUG allow to dray AOI rectangle ans gazePoint
global Y_KEY;
global N_KEY;
global E_KEY;

FAM_ATTENTION_GETTER_DURATION = 2*SCREEN_FRAME_RATE;  % time (in frames) looking attention getter to skip
FAM_ATTENTION_GETTER_MAX_DURATION = 10; % time (in sec)to skip attention getter, althoug is not looking
FAM_STIM_DURATION = 12; %secs
FAM_STIM_LOOK_AWAY = 1*SCREEN_FRAME_RATE; % (frames)

variableNames={'SystemTimeStamp','meanX','meanY','meanValidity','lX','lY','lV','lPupil','lPupilV','rX','rY','rV','rPupil','rPupilV','process', 'extra'};
[screenXpixels, screenYpixels] = Screen('WindowSize', window);


%% Select & prepare Stim pairs Image(1) Sound(2)
IMAGE_INDEX = 1;
SOUND_INDEX = 2;

stim_list1 = {"blue_dino.png"    "dumedu.wav"; ...
            "blue_dino.png"     "samesa.wav"; ...
            "blue_dino.png"     "lopulo.wav"; ...
            "blue_dino.png"     "sapusa.wav"; ...
            "red_car.png"       "dudume.wav"; ...
            "red_car.png"       "sasame.wav"; ...
            "red_car.png"       "lolopu.wav"; ...
            "red_car.png"       "sasapu.wav"; ...
            "blue_dino.png"     "dumedu.wav"; ...
            "red_car.png"       "dudume.wav"; ...
            "blue_dino.png"     "samesa.wav"; ...
            "red_car.png"       "sasame.wav"};

stim_list2 = {"red_car.png" "dumedu.wav"; ...
            "red_car.png"   "samesa.wav"; ...
            "red_car.png"   "lopulo.wav"; ...
            "red_car.png"   "sapusa.wav"; ...
            "blue_dino.png" "dudume.wav"; ...
            "blue_dino.png" "sasame.wav"; ...
            "blue_dino.png" "lolopu.wav"; ...
            "blue_dino.png" "sasapu.wav"; ...
            "red_car.png"   "dumedu.wav"; ...
            "blue_dino.png" "dudume.wav"; ...
            "red_car.png"   "samesa.wav"; ...
            "blue_dino.png" "sasame.wav"};
        
if condition==1
    stim_list=stim_list1;
else    
    stim_list=stim_list2;
end

% load stim images
% Import image and and convert it, stored in
% MATLAB matrix, into a Psychtoolbox OpenGL texture using 'MakeTexture';
for stimIndex = 1:size(stim_list,1)
    imgfileName = strcat(STIM_PATH, string(stim_list(stimIndex, IMAGE_INDEX)));
    [img, ~, alpha] = imread(imgfileName, 'png');
    % Crop image if it is larger then screen size. There's no image scaling
    % in maketexture
    [iy, ix, ~] = size(img);
    img(:, :, 4) = alpha;
    TEXTURES(stimIndex).name = imgfileName;
    TEXTURES(stimIndex).texture = Screen('MakeTexture', window, img);
    TEXTURES(stimIndex).iRect = [0 0 ix iy];
    TEXTURES(stimIndex).looking_count = 0;
    TEXTURES(stimIndex).no_looking_count = 0;
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
imgfileName = strcat(STIM_PATH, "attention_getter2.png");
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
for stimIndex = 1:size(stim_list,1)
    %% attention_getter
    ATTENTION_GETTER.centerRect = CenterRectOnPoint(ATTENTION_GETTER.iRect, centerX, centerY);
    tIni = tic;
    ATTENTION_GETTER.looking_count = 0;
    while toc(tIni)<FAM_ATTENTION_GETTER_MAX_DURATION
        Screen('AsyncFlipEnd', monitorWindow);
        ATTENTION_GETTER.centerRect = CenterRectOnPoint(ATTENTION_GETTER.iRect*ATTENTION_GETTER.zoom, centerX, centerY);
        Screen('DrawTexture', window, ATTENTION_GETTER.texture, ATTENTION_GETTER.iRect, ATTENTION_GETTER.centerRect, ATTENTION_GETTER.rotation);
        Screen('DrawTexture', monitorWindow, ATTENTION_GETTER.texture, ATTENTION_GETTER.iRect, ATTENTION_GETTER.centerRect/SCREEN_MONITOR_PROPORTION, ATTENTION_GETTER.rotation);
        ATTENTION_GETTER.rotation = ATTENTION_GETTER.rotation + STEEP_ROTATION;
        ATTENTION_GETTER.zoom = 0.5+sin(ATTENTION_GETTER.rotation/100)/4;
        gaze_data = eyetracker.get_gaze_data();
        full_gaze_data = [full_gaze_data; parseGazeData(gaze_data, ["familiarization", strcat(ATTENTION_GETTER.name)])];
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
            if (ATTENTION_GETTER.looking_count>FAM_ATTENTION_GETTER_DURATION)
                break;
            end
        end
        Screen('AsyncFlipBegin', monitorWindow);
        Screen('Flip', window);
        [ keyIsDown, ~, keyCode ] = KbCheck;
        if find(keyCode, 1)== E_KEY
            throw(MException('Generalization:warning', 'End by experimenter: inside familiarization'))
        end
    end
    
    %w2 = (TEXTURES(textureIndex).iRect(3)-TEXTURES(textureIndex).iRect(1))/2;
    %h2 = (TEXTURES(textureIndex).iRect(4)-TEXTURES(textureIndex).iRect(2))/2;
    %AOI = [screen_center(1)-w2 screen_center(2)-h2 screen_center(1)+w2 screen_center(2)+h2];
    PsychPortAudio('FillBuffer', pAudioHandle, WAVEDATA(stimIndex).data);
%     PsychPortAudio('Start', pAudioHandle, 0, 0, 1); % repeat sound "for ever"
%     volume = PsychPortAudio('Volume', pAudioHandle, 1.0);
    PsychPortAudio('Start', pAudioHandle);
    AOI = CenterRectOnPoint(TEXTURES(stimIndex).iRect, centerX, centerY);
    tIni = tic;
    rotation = 0;
    steep_rotation =- STEEP_ROTATION;
    while toc(tIni)<FAM_STIM_DURATION
        Screen('AsyncFlipEnd', monitorWindow);
        Screen('DrawTexture', window, TEXTURES(stimIndex).texture, TEXTURES(stimIndex).iRect, AOI, rotation);
        Screen('DrawTexture', monitorWindow, TEXTURES(stimIndex).texture, TEXTURES(stimIndex).iRect, AOI/SCREEN_MONITOR_PROPORTION, rotation);
        
        gaze_data = eyetracker.get_gaze_data();
        %full_gaze_data = parseGazeData(gaze_data);
        full_gaze_data = [full_gaze_data; parseGazeData(gaze_data, ["familiarization", strcat(TEXTURES(stimIndex).name, ",", WAVEDATA(stimIndex).name)])];
        if ~isempty(gaze_data)
            [x, y] = lastGazeData(gaze_data);
            x = x*screenXpixels;
            y = y*screenYpixels;
            if inrect([x y], AOI)
                %animation
                if DRAW_CUES
                    Screen('FrameRect', window, [0 255 0], AOI, 5);
                end
                Screen('FrameRect', monitorWindow, [0 255 0], AOI/SCREEN_MONITOR_PROPORTION, 5);
                if rotation>LIMIT_ROTATION || rotation<-LIMIT_ROTATION 
                    steep_rotation=-steep_rotation; 
                end
                rotation=rotation+steep_rotation;
                status = PsychPortAudio('GetStatus', pAudioHandle);
                if ~status.Active
                    PsychPortAudio('Start', pAudioHandle);
                end
                % audio volume UP
%                 if volume<1.0
%                     if volume>0.8
%                         volume = 1;
%                         PsychPortAudio('Start', pAudioHandle, 0, 0, 1); % repeat sound "for ever"
%                     else
%                         volume = volume+STEEP_SOUND_LEVEL_UP;
%                     end
%                 end
                TEXTURES(stimIndex).looking_count = TEXTURES(stimIndex).looking_count + 1;
            else
                % audio volume DOWN
%                 if volume>0
%                     if volume<0.2
%                         volume = 0;
%                         PsychPortAudio('Stop', pAudioHandle);
%                     else
%                         volume = volume-STEEP_SOUND_LEVEL_DOWN;
%                     end
%                 end
                TEXTURES(stimIndex).no_looking_count = TEXTURES(stimIndex).no_looking_count + 1;
            end
            if (TEXTURES(stimIndex).no_looking_count>FAM_STIM_LOOK_AWAY)
                break;
            end
            
%             PsychPortAudio('Volume', pAudioHandle , volume);
            if DRAW_CUES
                Screen('DrawDots', window, [x y], 80, EYE_DOT_COLOR, [], 2);
            end
            Screen('DrawDots', monitorWindow, [x y]/SCREEN_MONITOR_PROPORTION, 80, EYE_DOT_COLOR, [], 2);
            [ keyIsDown, ~, keyCode ] = KbCheck;
            if find(keyCode, 1)== E_KEY
                throw(MException('Generalization:warning', 'End by experimenter: inside familiarization'))
            end
        end
        
        Screen('AsyncFlipBegin', monitorWindow);
        Screen('Flip', window);
        
    end
%     while volume>0
%         volume = PsychPortAudio('Volume', pAudioHandle , volume-STEEP_SOUND_LEVEL_DOWN);
%         pause(0.01);
%     end
%     PsychPortAudio('Stop', pAudioHandle);
    % wait till audio finalizes
    status = PsychPortAudio('GetStatus', pAudioHandle);
    while status.Active
        status = PsychPortAudio('GetStatus', pAudioHandle);
        pause(0.1);
    end
end

eyetracker.stop_gaze_data();
PsychPortAudio('Stop', pAudioHandle);

Screen('AsyncFlipEnd', monitorWindow);
Screen('Flip', window);

sec=1/Screen('NominalFrameRate', window);
calculated = '';
for i = 1:size(stim_list, 2)
    tmpStr = sprintf('Image: %s\t\t Sound: %s\t\t looked for: %.2f seconds\r\n', TEXTURES(i).name, WAVEDATA(i).name, TEXTURES(i).looking_count*sec );
    calculated = [calculated, tmpStr];
end

end

