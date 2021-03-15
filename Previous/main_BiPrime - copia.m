%% Basic Sample to use Tobii Eye Trackers with TobiiPro.SDK.Matlab_1.6.1.21
% Code following Guide: http://developer.tobiipro.com/matlab/matlab-sdk-reference-guide.html
%
%
% 20190516 ...by xMayoral + sBlanch
%
% INITIALIZATION
%  Global configuration: paths, vars...
%  SETUP Tobii
%   setup path
%   look for eyetrackers
%  SETUP PsychToolBox
%   graphics
%   audio
% Possitioning subjent in front eyeTracker
% Calibrate eyeTracker for subject eyes
% START Study
%  Show an image and get eye data
% FINALIZATION
%
% PRESS 'e' to exit at any moment

%% [INI] Initialization

% % Clear and basic scripte Set-Up
% sca;
% close all;

% clear the workspace and the screen --------------------------------------
sca
clc
clearvars;
close all;
delete(instrfind);
warning('off') %if you do not want to see warnings in command window


ME = [];    % To keep track of possible failures
try         % and enable a possible "clean" exit
    
    % Add access to the specific funtions used by Generalization2 Study
    folder = strcat(pwd, filesep, "Functions");
    addpath(genpath(folder));
    % Add access to all the Tobii SDK functions
    folder = strcat(pwd, filesep, "TobiiPro.SDK.Matlab_1.6.1.21");
    addpath(genpath(folder));
    
    % set paths GONZALO ------------------------------------------------------------
    mainDirectory = 'C:\Users\cbclab\Documents\Gonzalo\BiPrime';
    cd(mainDirectory);
    
    %gettersImagePath = [mainDirectory '\getterImage.jpg']; % provisional
    listsPath   = [mainDirectory '\lists\'];
    gettersPath = [mainDirectory '\getters\'];
    imagesPath  = [mainDirectory '\images\'];
    audiosPath  = [mainDirectory '\audios\'];
    
    % Global variables used acros funtions
    % % %%
    % global DRAW_CUES;   %for DEBUG allow to dray AOI rectangle ans gazePoint
    % DRAW_CUES = true;   %for DEBUG allow to draw AOI rectangle and gazePoint
    % Global variables used acros funtions
    global STIM_PATH;
    global RES_PATH;
    %global GETTER_DURATION;
    %global PRIME_STIM_DURATION;
    global STEEP_SOUND_LEVEL_UP;
    global STEEP_SOUND_LEVEL_DOWN;
    global LIMIT_ROTATION;
    global STEEP_ROTATION;
    global TEXT_COLOR;
    global TEXT_SIZE;
    global BACKGROUND_COLOR;
    global SCREEN_WIDTH;
    global SCREEN_HEIGHT;
    global SCREEN_FRAME_RATE;
    global EYE_DOT_COLOR;
    global SCREEN_MONITOR_PROPORTION; % controls the size of the screen experimenter monitor
    global DRAW_CUES;   %for DEBUG allow to dray AOI rectangle ans gazePoint
    global Y_KEY;
    global N_KEY;
    global E_KEY;
    
    STIM_PATH = strcat(pwd, filesep, 'Stim', filesep);
    RES_PATH = strcat(pwd, filesep, 'Res', filesep);
    %GETTER_DURATION = 3;
    %PRIME_STIM_DURATION = 1.5;
    STEEP_SOUND_LEVEL_UP = 0.1;
    STEEP_SOUND_LEVEL_DOWN = 0.1;
    LIMIT_ROTATION = 25;
    STEEP_ROTATION = 1;
    TEXT_COLOR = [0 0 0];
    TEXT_SIZE = 30;
    BACKGROUND_COLOR = [128 128 128];
    SCREEN_WIDTH = 1920;
    SCREEN_HEIGHT = 1080;
    SCREEN_FRAME_RATE = 60;
    EYE_DOT_COLOR = [255 255 0];
    SCREEN_MONITOR_PROPORTION = 2;
    % %%%%%%%%
    DRAW_CUES = false;   %for DEBUG allow to dray AOI rectangle ans gazePoint
    %
    
    Y_KEY = KbName('Y');
    N_KEY = KbName('N');
    E_KEY = KbName('E');
    
    % keys GONZALO --------------------------------------------------------------------
    KbName('UnifyKeyNames');    % unify the names of keys across operating systems
    keyR     = KbName('r');     % key for recalibrate
    keySpace = KbName('space'); % key for signaling trial onset
    keyQ     = KbName('q');     % key for quitting
    
    xCenter = SCREEN_WIDTH/2;
    yCenter = SCREEN_HEIGHT/2;
    
    
    %%% INPUT GONZALO
    %% login prompt
    prompt         = {'file', 'sujenum','id', 'date_test', 'name','date_birth', 'sex', 'language', 'list', 'comments'};     % define prompt variables
    %languagelist = {'Catalan','Spanish'};
    %[indx,tf] = listdlg('ListString',languagelist);
    promptDefaults = {'bprime', '','000', datestr(now, 'yyyy-mm-dd_HH-MM'),'NA','00-00-00', 'NA', 'NA', 'NA', 'NA'}; % define prompt default values
    promptAnswer   = inputdlg(prompt, 'BiPrime', 2, promptDefaults);                                  % define prompt properties
    
    [output, sujenum, id, date_test, name,date_birth, sex, language, list, comments] = deal(promptAnswer{:});            % all variables are strings
    filename       = [output sujenum '_' id '_' name '_' date_test  '_birth_' date_birth '_' language list '.csv'];                               % output file name
    filepath       = cd;
    resFileName    = fullfile(RES_PATH, filename);
    
    %%%%%%%IF YOU WANT A DIFERENT FOLDER FOR EACH SUBJECT
    FolderName = [sujenum '_' id '_' name];
    mkdir(RES_PATH,FolderName);
    resFileName = fullfile([RES_PATH '\' FolderName '\'], filename);
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%END FOLDER SUBJECT
    
    fid = fopen(resFileName, 'a+'); % open a file for writing data out
    variableNames={'SystemTimeStamp','meanX','meanY','meanValidity','lX','lY','lV','lPupil','lPupilV','rX','rY','rV','rPupil','rPupilV','language','list','numtrial_lista','trial','process','extra'};
    
    filenameLOG = [RES_PATH FolderName '\' output sujenum '_' id '_' name '_' date_test '_' language '_' list 'LOG.txt'];
    %logFileID = fopen(strcat(RES_PATH, id ,'log.txt'), 'a');e
    logFileID = fopen(filenameLOG, 'wt');
    fprintf(logFileID,'START: %s\n', datestr(now, 'dd/mm/yyyy HH:MM:SS'));
    fprintf(logFileID, 'sujenum: %s\n',sujenum);    
    fprintf(logFileID, 'id: %s\n',id);    
    fprintf(logFileID, 'name: %s\n',name);
    fprintf(logFileID, 'date_test %s\n',date_test);
    fprintf(logFileID, 'date_birth %s\n',date_birth);
    fprintf(logFileID, 'sex %s\n',sex);
    fprintf(logFileID, 'language %s\n',language);
    fprintf(logFileID, 'list %s\n',list);
    fprintf(logFileID, 'comments %s\n',comments);
    
    if fid == -1
        error('Cannot open file for writing: %s', resFileName);
    end
    
    %%%%% FI INPUT GONZALO
    
    % EyeTracker Set Up
    disp('[INI] Set up eye Tracker');
    eyetracker = eyeTrackerInitialize();
    disp('[END] Set up eye Tracker');
    
    % Default settings for setting up Psychtoolbox
    PsychDefaultSetup(2);
    
    % Audio Set Up
    disp('[INI] Set up audio');
    pAudioHandle = audioInitialize();
    disp('[END] Set up audio');
    
    % Initialize graphics mode
    disp('[INI] Set uep graphics');
    [eyeTrackerWindow, monitorWindow] = graphicsInitialize();
    disp('[END] Set up graphics');
    
    %--------------------------------------------------------------------------
    % LIGHTS SETUP & OPEN & CLEAN
    %--------------------------------------------------------------------------
    priorPorts = instrfind({'Port'},{'COM3'}) ;
    delete(priorPorts); % and deletes them
    
    ser = serial('COM3','BaudRate', 115200);
    fopen(ser);
    fwrite(ser, 'QWERTYUI'); %borrem tot
    
    %--------------------------------------------------------------------------
    % RESTART RANDOMIZE FUNCTIONS
    %--------------------------------------------------------------------------
    s = RandStream.create('mt19937ar','seed',sum(100*clock));
    % RandStream.setDefaultStream(s);
    RandStream.setGlobalStream(s);
    
%     %% java heap memory GONZALO
%     heapTotalMemory = java.lang.Runtime.getRuntime.totalMemory;
%     heapFreeMemory  = java.lang.Runtime.getRuntime.freeMemory;
%     if(heapFreeMemory < (heapTotalMemory*0.01))
%         java.lang.Runtime.getRuntime.gc;
%     end
    
    % [END] Initialization
    
    %% [INI] Set up position in front eye Tracker
    disp('[INI] Set up position in front eye Tracker');
    disp('Check possition?[y,n,e]');
    
    DrawFormattedText(monitorWindow, 'Check possition?[y,n,e]', 'center', 'center', TEXT_COLOR);
    Screen('Flip', monitorWindow);
    DrawFormattedText(eyeTrackerWindow, 'Check possition?[y,n,e]', 'center', 'center', TEXT_COLOR);
    Screen('Flip', eyeTrackerWindow);
    
    keyCode = waitKeyPress();
    if keyCode == Y_KEY
        fprintf(logFileID,'POSITION: %s\n', datestr(now, 'dd/mm/yyyy HH:MM:SS'));
        positionBB(eyetracker, eyeTrackerWindow, monitorWindow, pAudioHandle);
    elseif keyCode == E_KEY
        throw(MException('Generalization:warning','End by experimenter: position'))
    end
    
    disp('[END] Set up position in front eye Tracker');
    % [END]  Set up position in front eye Tracker
    
    %-----------------------------------
    %% [INI] Calibration
    disp('[INI] Calibration');
    disp('Calibrate?[y,n,e]');
    
    DrawFormattedText(eyeTrackerWindow, 'Calibrate?[y,n,e]', 'center', 'center', TEXT_COLOR);
    Screen('Flip', eyeTrackerWindow);
    DrawFormattedText(monitorWindow, 'Calibrate?[y,n,e]', 'center', 'center', TEXT_COLOR);
    Screen('Flip', monitorWindow);
    
    keyCode = waitKeyPress();
    if keyCode == Y_KEY
        fprintf(logFileID,'CALIBRATE: %s\n', datestr(now, 'dd/mm/yyyy HH:MM:SS'));
        calibrateBB(eyetracker, eyeTrackerWindow, monitorWindow, pAudioHandle,resFileName);
    elseif keyCode == E_KEY
        throw(MException('Generalization:warning','End by experimenter: calibrate'))
    end
    
    disp('[END] Calibration');
    % [END] Calibration
    %-----------------------------------
    
    % timings GONZALO -----------------------------------------------------------------
    monitorFlipInterval=Screen('GetFlipInterval',eyeTrackerWindow);
    t3000 = (ceil(3.0 / monitorFlipInterval) - 0.5) * monitorFlipInterval; % 3000 ms
%     t4500 = (ceil(4.5 / monitorFlipInterval) - 0.5) * monitorFlipInterval; % 4500 ms
%     t4700 = (ceil(4.7 / monitorFlipInterval) - 0.5) * monitorFlipInterval; % 4700 ms
%     t5700 = (ceil(5.7 / monitorFlipInterval) - 0.5) * monitorFlipInterval; % 5700 ms
%     t7700 = (ceil(7.7 / monitorFlipInterval) - 0.5) * monitorFlipInterval; % 7700 ms
%     
%     PRIME_IMAGE_DURATION=t4500-t3000;
%     BLANK_DURATION=t4700-t3000;
%     AUDIO_DURATION=t5700-t3000;
%     TOTAL_DURATION=t7700-t3000;
    
    PRIME_IMAGE_DURATION=(ceil(1.5 / monitorFlipInterval) - 0.2) * monitorFlipInterval; % 1500 ms
    BLANK_DURATION=(ceil(1.55 / monitorFlipInterval) - 0.2) * monitorFlipInterval; % 50 ms
    AUDIO_DURATION=(ceil(2.25 / monitorFlipInterval) - 0.2) * monitorFlipInterval; % 700 ms
    TOTAL_DURATION=(ceil(4.25 / monitorFlipInterval) - 0.2) * monitorFlipInterval; % 2000 ms       
    
    Screen('Preference', 'SkipSyncTests', 0);
    
    
    %% [INI] BIPRIMING
    disp('[INI] BIPRIMING');
    
    %% lectura variables trial
    %GETTER
    
    %LIST
    [num, words]     = xlsread([listsPath 'list_' language list '.xlsx']);
    nTrials          = size(words, 1);
    arrayOrder       = 1:nTrials;
    arrayOrderRandom = Shuffle(arrayOrder);    
    
    % SCREEN_WIDTH = 1920;
    % SCREEN_HEIGHT = 1080;
    [screenXpixels, screenYpixels] = Screen('WindowSize', eyeTrackerWindow);
    windowRecteyetracker=Screen('Rect', eyeTrackerWindow);
    windowRectmonitor=Screen('Rect', monitorWindow);
    %windowRectGetter=[((SCREEN_WIDTH/2)-300) ((SCREEN_HEIGHT/2)-300) ((SCREEN_WIDTH/2)-300)+600 ((SCREEN_HEIGHT/2)-300)+600];
    windowRectGetter=[((SCREEN_WIDTH/2)-75) ((SCREEN_HEIGHT/2)-75) ((SCREEN_WIDTH/2)-75)+150 ((SCREEN_HEIGHT/2)-75)+150];
    %AOIRectGetter=[((SCREEN_WIDTH/2)-200) ((SCREEN_HEIGHT/220075) ((SCREEN_WIDTH/2)-200)+400 ((SCREEN_HEIGHT/2)-200)+400];
    
    
    %%empty the eyeTracker buffer
    eyetracker.get_gaze_data();
    full_gaze_data = [];
    
    %%%%%%%%%%%%%%%%%%%%%%
    for itrial = 1:nTrials % for each trial until nTtrials
        
        % if Q is pressed, break the loop
        [keyIsDown, secs, keyCode] = KbCheck;
        if (keyIsDown && keyCode(keyQ))
            break;
        end
        
        %ii = arrayOrderRandom(i)+1; % get a random trial from list GON?
        ii = arrayOrderRandom(itrial);
        
        % for the i-th  trial -------------------------------------------------
        primeImagePath      = [imagesPath  char(words(ii, 1)) '.jpg']; % get the ii-th prime image
        targetImagePath     = [imagesPath  char(words(ii, 2)) '.jpg']; % get the ii-th target image
        distractorImagePath = [imagesPath  char(words(ii, 3)) '.jpg']; % get the ii-th distractor image
        targetAudioPath     = [audiosPath  char(words(ii, 2)) '.wav']; % get the ii-th target audio
        
        
        %% [INI] GETTERS
        disp(['[INI ' num2str(itrial) '] GETTERS']);
        numgetter=(randi(8));
        %numgetter=8;
        
        mivideo   = [ gettersPath '\attention' num2str(numgetter) 's.mov'];
        info      = mmfileinfo(mivideo);
        vid       = VideoReader(mivideo);
        duration  = vid.Duration;
        framerate = vid.FrameRate;
        numFrames = duration*framerate;
        n         = floor(numFrames);
        
        FolderName       = vid.Name;
        FolderName       = FolderName(1:(length(FolderName)-4));
        topPriorityLevel = MaxPriority(eyeTrackerWindow);
        numSecs          = 1;
        numFrames        = round(numSecs / monitorFlipInterval);
        waitframes       = 1/framerate;
        
        [y,Fs] = audioread(mivideo);
        nrchannels = size(y,2);
        if (numgetter == 3)
           y = [y y]; 
        end
        
        %llegim primera imatge
        imdata       = imread([gettersPath FolderName '\' 'Image' num2str(0) '.jpg']);
        [iy, ix, id] = size(imdata);
        iRect        = [0 0 ix iy];
        textureIndex = Screen('MakeTexture', eyeTrackerWindow, imdata);  %IMAGE PREPARE TEXTURE
        Screen('DrawTexture', eyeTrackerWindow, textureIndex, iRect, windowRectGetter);
        
        %llegim imatge10 per monitor screen
        imdata       = imread([gettersPath FolderName '\' 'Image' num2str(10) '.jpg']);
        [iy, ix, id] = size(imdata);
        iRect        = [0 0 ix iy];
        %textureIndex=Screen('MakeTexture', WindowIndex, imageMatrix 
        textureIndex = Screen('MakeTexture', monitorWindow, imdata);  %IMAGE PREPARE TEXTURE
        %Screen('DrawTexture', windowPointer, texturePointer [,sourceRect] [,destinationRect]
        Screen('DrawTexture', monitorWindow, textureIndex, iRect, windowRectGetter/2);        
        
        restem = monitorFlipInterval/4;
        Priority(topPriorityLevel);
        
        wavedata = y';
        repetitions=1;
        PsychPortAudio('FillBuffer', pAudioHandle, wavedata);
        PsychPortAudio('Start', pAudioHandle, repetitions, 0, 1);
        inicigetter=tic;
        vbl = Screen('Flip', eyeTrackerWindow);
        firstFlip = vbl;
        vbl = Screen('Flip', monitorWindow);
        buclegetter = true;
        igetter = 1;
        numflipsmirant=0;
        while buclegetter        
        %for igetter=1:n-1
            imdata = imread([gettersPath FolderName '\' 'Image' num2str(igetter) '.jpg']);
            [iy, ix, id]=size(imdata);
            iRect = [0 0 ix iy];
            textureIndex = Screen('MakeTexture', eyeTrackerWindow, imdata);  %IMAGE PREPARE TEXTURE
            Screen('DrawTexture', eyeTrackerWindow, textureIndex, iRect, windowRectGetter);
            Screen('DrawTexture', monitorWindow, textureIndex, iRect, windowRectGetter/2);
            
            %Screen('AsyncFlipEnd', monitorWindow);
            gaze_data = eyetracker.get_gaze_data();
            [x, y] = lastGazeData(gaze_data); %x,y are from 0..1
            x = x*screenXpixels; % scale to screen size
            y = y*screenYpixels;
            %Screen('DrawDots', windowPtr, xy [,size] [,color] [,center] [,dot_type][, lenient]);
            Screen('DrawDots', monitorWindow, [x y]/SCREEN_MONITOR_PROPORTION, 40, EYE_DOT_COLOR, [], 2);   
                        
            if DRAW_CUES                
                Screen('DrawDots', eyeTrackerWindow, [x y], 40, EYE_DOT_COLOR, [], 2);                
            end
            
            vbl = Screen('Flip', eyeTrackerWindow, vbl + waitframes - 0.0001);
            Screen('Flip', monitorWindow); % display PRIME IMAGE
            
            if inRect([x y], windowRectGetter)   
                numflipsmirant = numflipsmirant+1
                if numflipsmirant>15                    
                    break
                end
            end
            
            if igetter== (n-1)
                 %igetter
                 igetter=1;
            else
                 igetter=igetter+1;
            end
                          
            if toc(inicigetter)>t3000
                 break
            end
        end %while buclegetter
        
        %%% [carga] PRIME IMAGE
        imdata = imread(primeImagePath);                          % read image file and make it a matrix
        [iy, ix, id] = size(imdata);                              % measure the matrix
        iRectPRIME = [0 0 ix iy];                                      % resize the matrix
        textureIndexPRIME = Screen('MakeTexture', eyeTrackerWindow, imdata);  % define matrix as an image
        %textureIndexPRIME = Screen('MakeTexture', monitorWindow, imdata);  % define matrix as an image
        
        %%% [carga] AUDIO
        % load stimuli
        [targetAudio, freq] = audioread(targetAudioPath);
        wavedata_ATT = targetAudio';
        nrchannels = size(wavedata_ATT,1); % Number of rows == number of channels.
        PsychPortAudio('FillBuffer', pAudioHandle, wavedata_ATT);
        
        %%% [carga] TARGET & DISTRACTOR
        tardisordre=randi(2);
        if (tardisordre)==1
            rightImagePath=targetImagePath;
            leftImagePath=distractorImagePath;
            lookText_TARDIS = [char(words(ii, 3)) '-' char(words(ii, 2))];
        else
            rightImagePath=distractorImagePath;
            leftImagePath=targetImagePath;
            lookText_TARDIS = [char(words(ii, 2)) '-' char(words(ii, 3))];
        end
        % prepare image to the right
        imdata = imread(rightImagePath);
        [iy, ix, id] = size(imdata);
        iRectR = [0 0 ix iy];                                      % resize the matrix
        textureIndexR = Screen('MakeTexture', eyeTrackerWindow, imdata);
        %textureIndexR = Screen('MakeTexture', monitorWindow, imdata);
        
        % prepare image to the left
        imdata = imread(leftImagePath);
        [iy, ix, id] = size(imdata);
        iRectL = [0 0 ix iy];
        textureIndexL = Screen('MakeTexture', eyeTrackerWindow, imdata);
        %textureIndexL = Screen('MakeTexture', monitorWindow, imdata);
        
        
        %WaitSecs('Untiltime',firstFlip + t3000);
        finalgetter=toc(inicigetter);    
        
        %a1 = tic; % start counting until toc
        %% [END] GETTERS        

        %% PRINT BACKGROUND SCREEN BOTH SCREENS
        Screen('FillRect', eyeTrackerWindow, BACKGROUND_COLOR);
        Screen('FillRect', monitorWindow, BACKGROUND_COLOR);
        Screen('Flip', eyeTrackerWindow);
        Screen('Flip', monitorWindow);  % display BACKGROUND_COLOR
        
        %PREPAREM PRIME IMAGE
        Screen('DrawTexture', monitorWindow, textureIndexPRIME, iRectPRIME,...  % prepare the image to be presented
            [xCenter/2-(ix/4) yCenter/2-(iy/4) xCenter/2+(ix/4) yCenter/2+(iy/4)]);
        Screen('DrawTexture', eyeTrackerWindow, textureIndexPRIME, iRectPRIME,...  % prepare the image to be presented
            [xCenter-(ix/2) yCenter-(iy/2) xCenter+(ix/2) yCenter+(iy/2)]);
        
        %%empty the eyeTracker buffer
        eyetracker.get_gaze_data();
       
        %% [INI] PRIME IMAGE
        disp(['[INI ' num2str(itrial) '] PRIME IMAGE']);
        % 2. prime image from 3000 to 4500 ms -----------------------------
        Screen('Flip', eyeTrackerWindow);
        Screen('Flip', monitorWindow);  % display BACKGROUND_COLOR
        tIni = tic; 
        numFLIPS=0;
        
        while toc(tIni)<=PRIME_IMAGE_DURATION
            
            %Screen('AsyncFlipEnd', monitorWindow);
            gaze_data = eyetracker.get_gaze_data();
            %lookText = strcat('LOOK_', char(words(ii, 1)));
            lookText_DIS = char(words(ii, 1));
            full_gaze_data = [full_gaze_data; parseGazeData(gaze_data, [language, list, num2str(num(ii,1)),itrial, "PRIMEIMAGE", lookText_DIS])];
            
            [x, y] = lastGazeData(gaze_data); %x,y are from 0..1
            x = x*screenXpixels; % scale to screen size
            y = y*screenYpixels;
                         
            [ keyIsDown, ~, keyCode ] = KbCheck;
            if find(keyCode, 1) == E_KEY
                throw(MException('PRIME_IMAGE:warning', 'End by experimenter: inside PRIME_IMAGE'))
            end
            
            %MONITOR WINDOW            
            Screen('DrawTexture', monitorWindow, textureIndexPRIME, iRectPRIME,...  % prepare the image to be presented
                [xCenter/2-(ix/4) yCenter/2-(iy/4) xCenter/2+(ix/4) yCenter/2+(iy/4)]);
            %Screen('DrawDots', windowPtr, xy [,size] [,color] [,center] [,dot_type][, lenient]);
            Screen('DrawDots', monitorWindow, [x y]/SCREEN_MONITOR_PROPORTION, 40, EYE_DOT_COLOR, [], 2);
            
            %EYETRACKER WINDOW
            Screen('DrawTexture', eyeTrackerWindow, textureIndexPRIME, iRectPRIME,...  % prepare the image to be presented
                [xCenter-(ix/2) yCenter-(iy/2) xCenter+(ix/2) yCenter+(iy/2)]);
            if DRAW_CUES
                Screen('DrawDots', eyeTrackerWindow, [x y], 40, EYE_DOT_COLOR, [], 2);
            end
            
            %Screen('AsyncFlipBegin', monitorWindow);
            Screen('Flip', eyeTrackerWindow);
            Screen('Flip', monitorWindow); % display PRIME IMAGE
            numFLIPS=numFLIPS+1;
            
        end %WHILE TOC PRIME IMAGE
        
       
%         size(full_gaze_data,1)
%         size(full_gaze_data_ALL,1)
%         full_gaze_data_ALL = [full_gaze_data_ALL; full_gaze_data];
        
        fiPRIME=toc(tIni);
        %fprintf(logFileID,'PRIME numflips %d SECONDS %8.2f\t\n',numFLIPS,fiPRIME);
        numFLIPSPRIME=numFLIPS;
        numFLIPS=0;
        
        
        %% [INI] BLANK
        disp(['[INI ' num2str(itrial) '] BLANK']);
        % 3. blank screen and audio from 4500 to 5700 ms (max) ------------
        % blank screen
               
        while toc(tIni)<=BLANK_DURATION            
            
            %Screen('AsyncFlipEnd', monitorWindow);
            gaze_data = eyetracker.get_gaze_data();
             lookText_DIS = "blank";%char(words(ii, 1));
            %full_gaze_data = [full_gaze_data; parseGazeData(gaze_data, [language, condition, itrial, "BLANK", lookText])];
            full_gaze_data = [full_gaze_data; parseGazeData(gaze_data, [language, list, num2str(num(ii,1)),itrial, "BLANK", lookText_DIS])];
            
            [x, y] = lastGazeData(gaze_data); %x,y are from 0..1
            x = x*screenXpixels; % scale to screen size
            y = y*screenYpixels;
            
            [ keyIsDown, ~, keyCode ] = KbCheck;
            if find(keyCode, 1) == E_KEY
                throw(MException('PRIME_IMAGE:warning', 'End by experimenter: inside BLANK'))
            end
            
            %MONITOR WINDOW BLANK
            Screen('FillRect', monitorWindow, BACKGROUND_COLOR);
            Screen('DrawDots', monitorWindow, [x y]/SCREEN_MONITOR_PROPORTION, 40, EYE_DOT_COLOR, [], 2);
                        
            %EYTRACKER WINDOW BLANK
            Screen('FillRect', eyeTrackerWindow, BACKGROUND_COLOR);
            if DRAW_CUES
                Screen('DrawDots', eyeTrackerWindow, [x y], 40, EYE_DOT_COLOR, [], 2);
            end
            %Screen('AsyncFlipBegin', monitorWindow);
            Screen('Flip', eyeTrackerWindow);
            Screen('Flip', monitorWindow);  
            numFLIPS=numFLIPS+1;
            
        end %END WHILE BLANK
        %% [END] BLANK
        
        fiblank=toc(tIni);        
        %fprintf(logFileID,'BLANK numflips %d SECONDS %8.2f\t\n',numFLIPS,fiblank);
        numFLIPSBLANK=numFLIPS;
        numFLIPS=0;
        
        %full_gaze_data_ALL = [full_gaze_data_ALL; full_gaze_data];
                
        %% [INI] AUDIO
        disp(['[INI ' num2str(itrial) '] AUDIO']);
        targetAudioFlip     = PsychPortAudio('Start', pAudioHandle, repetitions, 0, 1); % play audio
        iniciaudio=toc(tIni);
        
        while toc(tIni)<=AUDIO_DURATION
            
            %Screen('AsyncFlipEnd', monitorWindow);
            gaze_data = eyetracker.get_gaze_data();
            lookText_DIS = "audio";%char(words(ii, 1));
            full_gaze_data = [full_gaze_data; parseGazeData(gaze_data, [language, list, num2str(num(ii,1)),itrial, "BLANK_AUDIO", lookText_DIS])];
                       
            [x, y] = lastGazeData(gaze_data); %x,y are from 0..1
            x = x*screenXpixels; % scale to screen size
            y = y*screenYpixels;                 
             
            [ keyIsDown, ~, keyCode ] = KbCheck;
            if find(keyCode, 1) == E_KEY
                throw(MException('PRIME_IMAGE:warning', 'End by experimenter: inside BLANK_AUDIO'))
            end
            
            %MONITOR WINDOW BACKGROUND
            Screen('FillRect', monitorWindow, BACKGROUND_COLOR);
            Screen('DrawDots', monitorWindow, [x y]/SCREEN_MONITOR_PROPORTION, 40, EYE_DOT_COLOR, [], 2);         
            
            %EYETRACKER WINDOW BACKGROUND
            Screen('FillRect', eyeTrackerWindow, BACKGROUND_COLOR);
            if DRAW_CUES
                Screen('DrawDots', eyeTrackerWindow, [x y], 40, EYE_DOT_COLOR, [], 2);
            end
            
            %Screen('AsyncFlipBegin', monitorWindow);
            Screen('Flip', eyeTrackerWindow);
            Screen('Flip', monitorWindow);
            numFLIPS=numFLIPS+1;
            
        end
        %% [END] AUDIO
        fiAudio=toc(tIni);        
        %fprintf(logFileID,'AUDIO numflips %d SECONDS %8.2f\t\n',numFLIPS,fiAudio);
        numFLIPSAUDIO=numFLIPS;
        numFLIPS=0;
        
        %full_gaze_data_ALL = [full_gaze_data_ALL; full_gaze_data];
        
        %% [INI] TARGET & DISTRACTOR
        disp(['[INI ' num2str(itrial) '] TARGET & DISTRACTOR']);
        % 5. target and distractor images from 5700 to 7700 ms ------------
        iniTARDIS=toc(tIni);
        
        while toc(tIni)<=TOTAL_DURATION
            
            %Screen('AsyncFlipEnd', monitorWindow);
            gaze_data = eyetracker.get_gaze_data();
            full_gaze_data = [full_gaze_data; parseGazeData(gaze_data, [language, list, num2str(num(ii,1)),itrial, "TARGET_DISTRACTOR", lookText_TARDIS])];
                        
            [x, y] = lastGazeData(gaze_data); %x,y are from 0..1
            x = x*screenXpixels; % scale to screen size
            y = y*screenYpixels;
            
            [ keyIsDown, ~, keyCode ] = KbCheck;
            if find(keyCode, 1) == E_KEY
                throw(MException('PRIME_IMAGE:warning', 'End by experimenter: inside TARGET_DISTRACTOR'))
            end        
               
            Screen('DrawTexture', monitorWindow, textureIndexL, iRectL, [(xCenter/4)-(ix/4) yCenter/2-(iy/4) (xCenter/4)+(ix/4) yCenter/2+(iy/4)]);
            Screen('DrawTexture', monitorWindow, textureIndexR, iRectR, [xCenter/2+(xCenter/4)-(ix/4) yCenter/2-(iy/4) xCenter/2+(xCenter/4)+(ix/4) yCenter/2+(iy/4)]);
            Screen('DrawDots', monitorWindow, [x y]/SCREEN_MONITOR_PROPORTION, 40, EYE_DOT_COLOR, [], 2);
            
            Screen('DrawTexture', eyeTrackerWindow, textureIndexL, iRectL, [(xCenter/2)-(ix/2)+50 yCenter-(iy/2) (xCenter/2)+(ix/2)+50 yCenter+(iy/2)]);
            Screen('DrawTexture', eyeTrackerWindow, textureIndexR, iRectR, [xCenter+(xCenter/2)-(ix/2)-50 yCenter-(iy/2) xCenter+(xCenter/2)+(ix/2)-50 yCenter+(iy/2)]);
                       
            if DRAW_CUES
                Screen('DrawDots', eyeTrackerWindow, [x y], 40, EYE_DOT_COLOR, [], 2);
            end
            
            %Screen('AsyncFlipBegin', monitorWindow);
            Screen('Flip', eyeTrackerWindow);
            Screen('Flip',monitorWindow);
            numFLIPS=numFLIPS+1;
            
            %full_gaze_data_ALL = [full_gaze_data_ALL; full_gaze_data];
            
        end
        %% FI TARGET & DISTRACTOR
        fiTARDIS=toc(tIni);         
        %fprintf(logFileID,'TARDIS numflips %d SECONDS %8.2f\t\n',numFLIPS,fiTARDIS);
        numFLIPSTARDIS=numFLIPS;
        numFLIPS=0;        
        
        %full_gaze_data_ALL = [full_gaze_data_ALL; full_gaze_data];
        disp('[END] BIPRIMING');
        
        % [END] BIPRIMING
        %-----------------------------------
        %fprintf(logFileID,'\n');
        fprintf(logFileID,'GETTER %8.2f\t PRIME-FLIPS %d\t PRIME-SEC %8.2f\t BLANK-FLIPS %d\t BLANK-SEC %8.2f\t AUDIO-FLIPS %d\t AUDIO-SEC %8.2f\t TARDIS-FLIPS %d\t TARDIS-SEC %8.2f\t\n',finalgetter,numFLIPSPRIME,fiPRIME,numFLIPSBLANK,fiblank,numFLIPSAUDIO,fiAudio,numFLIPSTARDIS,fiTARDIS);
        %writetable(array2table(full_gaze_data, 'VariableNames', variableNames), resFileName);
        
    end % end for trial
    
    writetable(array2table(full_gaze_data, 'VariableNames', variableNames), resFileName);
    
    eyetracker.stop_gaze_data();
    PsychPortAudio('Stop', pAudioHandle);
    
    %% [INI] CLEAR LIGHTS
    fwrite(ser, 'QWERTYUI'); %borrem abans de la cross
    fclose(ser); %close serial port
    delete(ser);
    clear ser;
    % [END] CLEAR LIGHTS
    
    %% [INI] CLEAR Psychotoolbox
    graphicsFinalize();
    audioFinalize(pAudioHandle);
    % [END] CLEAR Psychotoolbox
    eyeTrackerFinalize(eyetracker);    
    
    Screen('CloseAll');
    fprintf(logFileID,'END: %s\n', datestr(now, 'dd/mm/yyyy HH:MM:SS'));
    fclose(logFileID);
    fclose(fid);
    

%% in case of error or forced exit
%do some stuff
catch ME
    
    Screen('CloseAll');
    fprintf(logFileID,'END: %s\n', datestr(now, 'dd/mm/yyyy HH:MM:SS'));
    fclose(logFileID);
    fclose(fid);
end

if ~isempty(ME)
    rethrow(ME);
end