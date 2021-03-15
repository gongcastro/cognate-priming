% Basic Sample to use Tobii Eye Trackers with TobiiPro.SDK.Matlab_1.6.1.21
% Code following Guide: http://developer.tobiipro.com/matlab/matlab-sdk-reference-guide.html
%
%qq
% 20190516 ... by xMayoral + sBlanch
%qq
% INITIALIZATIONq, vars...
%  SETUP Tobiiqqq

%   setup path
%   look for eyetraqqckers
%  SETUP PsychToolBox
%   graphics
%   audio
% Possitioning subjent in front eyeTracker
% Calibrate eyeTracker for subject eyes
% START Study
%  Show an image and get eye dataq
% FINALIZATION
%
% PRESS 'e' to exit at any moment

%% [INI] Initialization

% % Clear and basic scripte Set-Up
% sca;
% close all;

% clear the workspace and the screen
% --------------------------------------
sca
clc
clearvars;
close all;
delete(instrfind);
warning('off') %if you do not want to see warnings in command window


ME = [];    % To keep track of possible failures
fullGazeData = []; %SIL2020
full_extra_cols = [];   


try         % and enable a possible "clean" exit
    
    folder = strcat(pwd, filesep, 'Functions');
    addpath(genpath(folder));
    % Add access to all the Tobii SDK functions
    folder = strcat(pwd, filesep, 'TobiiPro.SDK.Matlab_1.6.1.21');
    addpath(genpath(folder));
    
    % set paths GONZALO ------------------------------------------------------------
    mainDirectory = 'C:\Users\cbclab\Documents\gGarciaCastro\CognatePriming';
    cd(mainDirectory);
    
    %gettersImagePath = [mainDirectory '\getterImage.jpg']; % provisional
    listsPath      = [mainDirectory '\Lists\'];
    gettersPath    = [mainDirectory '\Stimuli\Getters\'];
    allImagesPath  = [mainDirectory '\Stimuli\Images\'];
    audiosPath     = [mainDirectory '\Stimuli\Sounds\'];
    
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
    
    STIM_PATH                 = strcat(mainDirectory, filesep, 'Stimuli', filesep);
    RES_PATH                  = strcat(mainDirectory, filesep, 'Data', filesep);
    %GETTER_DURATION          = 3;
    %PRIME_STIM_DURATION      = 1.5;
    STEEP_SOUND_LEVEL_UP      = 0.1;
    STEEP_SOUND_LEVEL_DOWN    = 0.1;
    LIMIT_ROTATION            = 25;
    STEEP_ROTATION            = 1;
    TEXT_COLOR                = [0 0 0];
    TEXT_SIZE                 = 30;
    BACKGROUND_COLOR          = [128 128 128];
    SCREEN_WIDTH              = 1920;
    SCREEN_HEIGHT             = 1080;
    SCREEN_FRAME_RATE         = 60;
    EYE_DOT_COLOR             = [255 255 0];
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
    prompt         = {'Experiment','Participant','ID','Participant name','Birth date','Sex','Language','Version','List', 'Comments'}; % define prompt variables
    promptDefaults = {'CognatePriming' '000', '', '', '0000-00-00', '', '', '', '', ''}; % define prompt default values

    while 1
        promptAnswer = inputdlg(prompt, 'CognatePriming', 2, promptDefaults);                              % define prompt properties
        [output,participant,id,name,dateBirth,sex,language,version,list,comments] = deal(promptAnswer{:}); % all variables are strings
        promptDefaults = {'CognatePriming', '000', '', '', '0000-00-00', '', '', '', '', ''}; % define prompt default values
        break
    end %while
                
    audiosPath    = [audiosPath    'sounds_'   language, '\'];
    imagesPath    = [allImagesPath 'images_'   language '\'];
    iPath         = [imagesPath 'images_'   language '\'];
    dateTest      = datestr(now, 'yyyy-mm-dd_HH-MM');
    filePath      = cd;
    filename      = [output '_' participant '_' datestr(now, 'yyyy-mm-dd')]; % output file name
    gazeDataPath  = [RES_PATH 'data_gaze\' , filename, '.csv']; % to save gaze data
    logDataPath   = [RES_PATH 'data_log\'  , filename, '.txt']; % to save log data
    calibDataPath = [RES_PATH 'data_calib\', filename, '.jpg']; % to save calibration data
    
    variableNames={'DeviceTimeStamp', 'SystemTimeStamp',...
        'l', 'lPointUserCoord',...
        'lV', 'lPupil', 'lPupilV',...
        'lOriginUserCoord', 'lOriginTrackboxCoord','lOriginV',...
        'r', 'rPointUserCoord',...
        'rV', 'rPupil', 'rPupilV',...
        'rOriginUserCoord', 'rOriginTrackboxCoord','rOriginV',...
        'Participant', 'TrialNum', 'Trial', 'TargetLocation', 'Phase'};
    
    % if files exist, add a number at the end of the filename until none of
    % them is duplicated.
    i = 0;
    while isfile(logDataPath) || isfile(gazeDataPath) || isfile(calibDataPath)
        i = i+1;
        filenameSuffix = [filename '_' num2str(i)];
        gazeDataPath   = [RES_PATH 'data_gaze\' , filenameSuffix, '.csv']; % to save gaze data
        logDataPath    = [RES_PATH 'data_log\'  , filenameSuffix, '.txt']; % to save log data
        calibDataPath  = [RES_PATH 'data_calib\', filenameSuffix, '.jpg']; % to save calibration data
    end
    
    fid = fopen(gazeDataPath, 'a+'); % open a file for writing data out
    logFileID = fopen(logDataPath, 'wt');
    fprintf(logFileID, 'Test date: %s\n', datestr(now, 'dd/mm/yyyy HH:MM:SS'));
    fprintf(logFileID, 'Attempt: %s\n', num2str(i+1));
    fprintf(logFileID, 'Participant: %s\n',participant);    
    fprintf(logFileID, 'ID: %s\n',num2str(id));    
    fprintf(logFileID, 'Name: %s\n',name);
    fprintf(logFileID, 'Birthdate: %s\n',dateBirth);
    fprintf(logFileID, 'Sex: %s\n',sex);
    fprintf(logFileID, 'Language: %s\n',language);
    fprintf(logFileID, 'List: %s\n',list);
    fprintf(logFileID, 'Version: %s\n',version);
    
    if fid == -1
        error('Cannot open file for writing: %s', gazeDataPath);
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
    disp('[INI] Set up graphics');
    [eyeTrackerWindow, monitorWindow] = graphicsInitialize();
    disp('[END] Set up graphics');
    
    %--------------------------------------------------------------------------
    % LIGHTS SETUP & OPEN & CLEAN
    %--------------------------------------------------------------------------
    priorPorts = instrfind({'Port'},{'COM3'}) ;
    delete(priorPorts); % and deletes them
    
    ser = serial('COM4','BaudRate', 115200);
    fopen(ser);
    fwrite(ser, 'QWERTYUI'); %borrem tot
    
    %fwrite(ser, '12345678'); %encendemos todo
    %fwrite(ser, '1'); %encendemos la primera solo
    
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
        throw(MException('Generalization: warning','End by experimenter: position'))
    end
    
    disp('[END] Set up position in front eye-tracker');
    % [END]  Set up position in front eye Tracker
    
    
    %% [INI] Calibration
    disp('[INI] Calibration');
    disp('Calibrate?[y,n,e]');
    
    DrawFormattedText(eyeTrackerWindow, 'Calibrate?[y,n,e]', 'center', 'center', TEXT_COLOR);
    Screen('Flip', eyeTrackerWindow);
    DrawFormattedText(monitorWindow, 'Calibrate?[y,n,e]', 'center', 'center', TEXT_COLOR);
    Screen('Flip', monitorWindow);
    
    keyCode = waitKeyPress();
    if keyCode == Y_KEY
        fprintf(logFileID,'Calibration time: %s\n', datestr(now, 'dd/mm/yyyy HH:MM:SS'));
        calibrateBB(eyetracker, eyeTrackerWindow, monitorWindow, pAudioHandle,calibDataPath);
    elseif keyCode == E_KEY
        throw(MException('Generalization: warning','End by experimenter: calibrate'))
    end
    
    disp('[END] Calibration');
    % [END] Calibration
    %-----------------------------------
    
    % timings GONZALO -----------------------------------------------------------------
    monitorFlipInterval = Screen('GetFlipInterval',eyeTrackerWindow);
    t3000 = (ceil(3.0 / monitorFlipInterval) - 0.5) * monitorFlipInterval; % 3000 ms
    t3000 = (ceil(3.0 / monitorFlipInterval) - 0.5) * monitorFlipInterval; % 3000 ms
%     t2500 = (ceil(2.5 / monitorFlipInterval) - 0.5) * monitorFlipInterval; % 3000 ms
%     t4500 = (ceil(4.5 / monitorFlipInterval) - 0.5) * monitorFlipInterval; % 4500 ms
%     t4700 = (ceil(4.7 / monitorFlipInterval) - 0.5) * monitorFlipInterval; % 4700 ms
%     t5700 = (ceil(5.7 / monitorFlipInterval) - 0.5) * monitorFlipInterval; % 5700 ms
%     t7700 = (ceil(7.7 / monitorFlipInterval) - 0.5) * monitorFlipInterval; % 7700 ms
    
    getterVideoDuration    = (ceil(3.00 / monitorFlipInterval) - 0.2) * monitorFlipInterval; % 3000 ms
    baselineImageDuration  = (ceil(1.00 / monitorFlipInterval) - 0.2) * monitorFlipInterval; % 1000 ms
    primeImageDuration     = (ceil(1.50 / monitorFlipInterval) - 0.2) * monitorFlipInterval; % 1500 ms
    blankDuration          = (ceil(1.55 / monitorFlipInterval) - 0.2) * monitorFlipInterval; % 50 ms
    audioDuration          = (ceil(2.25 / monitorFlipInterval) - 0.2) * monitorFlipInterval; % 700 ms
    totalDuration          = (ceil(4.25 / monitorFlipInterval) - 0.2) * monitorFlipInterval; % 2000 ms       
    
    Screen('Preference', 'SkipSyncTests', 0);

    %% [INI] COGNATE PRIMING
    disp('[INI] COGNATE PRIMING');
    
    %% read trial variables
    % LIST
    [num, words]     = xlsread([listsPath 'list_' language '\' version '\' 'list_' language list '-' version '.xlsx']); % read list
    nTrials          = size(words, 1);                                     % calculate number of trials
    arrayOrder       = 1:nTrials;                                          % vector with trial numbers
    arrayOrderRandom = Shuffle(arrayOrder);                                % randomise trial order
    
    % SCREEN_WIDTH = 1920;
    % SCREEN_HEIGHT = 1080;
    [screenXpixels, screenYpixels] = Screen('WindowSize', eyeTrackerWindow);
    windowRecteyetracker           = Screen('Rect', eyeTrackerWindow);
    windowRectmonitor              = Screen('Rect', monitorWindow);
    % windowRectGetter             = [((SCREEN_WIDTH/2)-300) ((SCREEN_HEIGHT/2)-300) ((SCREEN_WIDTH/2)-300)+600 ((SCREEN_HEIGHT/2)-300)+600];
    windowRectGetter               = [((SCREEN_WIDTH/2)-75) ((SCREEN_HEIGHT/2)-75) ((SCREEN_WIDTH/2)-75)+150 ((SCREEN_HEIGHT/2)-75)+150];
    % AOIRectGetter                = [((SCREEN_WIDTH/2)-200) ((SCREEN_HEIGHT/220075) ((SCREEN_WIDTH/2)-200)+400 ((SCREEN_HEIGHT/2)-200)+400];
    
    %% empty the eyeTracker buffer
    eyetracker.get_gaze_data('flat');
%     fullGazeData = []; %SIL2020
%     full_extra_cols = [];
    %CONSTANTS.extraCols_variableNames = {'phase', 'attGetter', 'centerImage', 'leftImage',
    %extra_cols = CONSTANTS.extra_cols;   
    CONSTANTS.extra_cols.participant = 'NA'; %SIL2020
    CONSTANTS.extra_cols.TrialNum = 'NA';
    CONSTANTS.extra_cols.Trial = 'NA';
    CONSTANTS.extra_cols.TargetLocation = 'NA';
    CONSTANTS.extra_cols.Phase = 'NA';
    extra_cols = CONSTANTS.extra_cols;

    
    %% start main routine
    %%%%%%%%%%%%%%%%%%%%%%
    disp('[INI] MAIN ROUTINE');
    for iTrial = 1:nTrials % for each trial until nTtrials
        
        % if Q is pressed, break the loop
        [keyIsDown, secs, keyCode] = KbCheck;
        if (keyIsDown && keyCode(keyQ))
            break;
        end
        
        %ii = arrayOrderRandom(i)+1; % get a random trial from list GON?
        ii = arrayOrderRandom(iTrial);
        %ii = iTrial; % sequential for testing

        
        % for the i-th  trial -------------------------------------------------
        primeImagePath      = [imagesPath   char(words(ii, 1)) '.jpg']; % get the ii-th prime image
        targetImagePath     = [imagesPath   char(words(ii, 2)) '.jpg']; % get the ii-th target image
        distractorImagePath = [imagesPath   char(words(ii, 3)) '.jpg']; % get the ii-th distractor image
        targetAudioPath     = [audiosPath   char(words(ii, 4))]; % get the ii-th target audio
        targetLocation      = char(words(ii, 5));                       % get the ii-th target locatiion
        
        %% [INI] GETTERS
        disp(['[INI ' num2str(iTrial) '] GETTER']);       
       
        mivideo   = [gettersPath 'attention_getter.mov'];
        info      = mmfileinfo(mivideo);
        vid       = VideoReader(mivideo);
        duration  = vid.Duration;
        framerate = vid.FrameRate;
        numFrames = duration*framerate;
        n         = floor(numFrames);
        
        folderName       = vid.Name;
        folderName       = folderName(1:(length(folderName)-4));
        topPriorityLevel = MaxPriority(eyeTrackerWindow);
        numSecs          = 1;
        numFrames        = round(numSecs / monitorFlipInterval);
        waitframes       = 1/framerate;
        
        [y,Fs]    = audioread(mivideo);
        nChannels = size(y, 2);
        
        nFlips = 0;
        
        % read image 1
        imData       = imread([gettersPath folderName '\' 'Image' num2str(0) '.jpg']);
        [iy, ix, id] = size(imData);
        iRect        = [0 0 ix iy];
        textureIndex = Screen('MakeTexture', eyeTrackerWindow, imData);  % IMAGE PREPARE TEXTURE
        Screen('DrawTexture', eyeTrackerWindow, textureIndex, iRect, windowRectGetter);
        
        %llegim imatge10 per monitor screen
        imData       = imread([gettersPath folderName '\' 'Image' num2str(10) '.jpg']);
        [iy, ix, id] = size(imData);
        iRect        = [0 0 ix iy];
        %textureIndex=Screen('MakeTexture', WindowIndex, imageMatrix 
        textureIndex = Screen('MakeTexture', monitorWindow, imData);  %IMAGE PREPARE TEXTURE
        %Screen('DrawTexture', windowPointer, texturePointer [,sourceRect] [,destinationRect]
        Screen('DrawTexture', monitorWindow, textureIndex, iRect, windowRectGetter/2);        
        
        restem = monitorFlipInterval/4;
        Priority(topPriorityLevel);
        
        wavedata    = y';
        repetitions = 1;
        PsychPortAudio('FillBuffer', pAudioHandle, wavedata);
        PsychPortAudio('Start', pAudioHandle, repetitions, 0, 1);
        tIni = tic;
        fwrite(ser,'1'); %getter
        vbl         = Screen('Flip', eyeTrackerWindow);
        firstFlip   = vbl;
        vbl         = Screen('Flip', monitorWindow);
        iGetter     = 1;
        
        fwrite(ser,'Q'); %getter
        
        disp('[GETTER loaded]')
        
        while 1        
        % for iGetter=1:n-1
            
            imData       = imread([gettersPath folderName '\' 'Image' num2str(iGetter) '.jpg']);
            [iy, ix, id] = size(imData);
            iRect        = [0 0 ix iy];
            textureIndex = Screen('MakeTexture', eyeTrackerWindow, imData);  %IMAGE PREPARE TEXTURE
            Screen('DrawTexture', eyeTrackerWindow, textureIndex, iRect, windowRectGetter);
            Screen('DrawTexture', monitorWindow, textureIndex, iRect, windowRectGetter/2);
            
            %Screen('AsyncFlipEnd', monitorWindow);
            gazeData = eyetracker.get_gaze_data('flat'); %SIL2020            
            %fullGazeData  = [fullGazeData; parseGazeData(gazeData, [participant,iTrial,num2str(num(ii,1)),words(ii,4),'Getter'])];
            extra_cols = CONSTANTS.extra_cols;
            extra_cols.participant=participant;
            extra_cols.TrialNum=iTrial;
            extra_cols.Trial=num2str(num(ii,1));
            extra_cols.TargetLocation=words(ii,4);
            extra_cols.Phase='Getter';      %SIL2020       
            fullGazeData = [fullGazeData; struct2table(gazeData)]; %SIL2020            
            full_extra_cols = [full_extra_cols; repmat(extra_cols, size(gazeData.device_time_stamp,1), 1)];%SIL2020     
            
            [x, y]   = lastGazeData(gazeData); %x,y are from 0..1            
            x        = x*screenXpixels; % scale to screen size
            y        = y*screenYpixels;
            %Screen('DrawDots', windowPtr, xy [,size] [,color] [,center] [,dot_type][, lenient]);
            Screen('DrawDots', monitorWindow, [x y]/SCREEN_MONITOR_PROPORTION, 40, EYE_DOT_COLOR, [], 2);   
                        
            if DRAW_CUES                
                Screen('DrawDots', eyeTrackerWindow, [x y], 40, EYE_DOT_COLOR, [], 2);                
            end
            
            vbl = Screen('Flip', eyeTrackerWindow, vbl + waitframes - 0.0001);
            Screen('Flip', monitorWindow); % display PRIME IMAGE
            
%             if inRect([x y], windowRectGetter)   
%                 nFlipsmirant = nFlipsmirant+1
%                 if nFlipsmirant>15                    
%                     break
%                 end
%             end
            
            if iGetter == (n-1)
                 %iGetter
                 iGetter = 1;
            else
                 iGetter = iGetter+1;
            end
                          
            if toc(tIni)>t3000
                 break
            end
            nFlips = nFlips+1;
        end %while getterLoop
        
        fiGetter     = toc(tIni);
        nFlipsGetter = nFlips;
        fprintf(logFileID,'GETTER nFlips %d SECONDS %8.2f\t\n', nFlipsGetter, fiGetter);
        nFlips       = 0;
        disp(['[END ' num2str(iTrial) '] GETTER']);        
        
        disp(['[INI ' num2str(iTrial) '] LOAD STIMULI']);
        
        %%% [LOAD] PRIME IMAGE
        imData            = imread(primeImagePath);                          % read image file and make it a matrix
        [iy, ix, id]      = size(imData);                                    % measure the matrix
        iRectPrime        = [0 0 ix iy];                                     % resize the matrix
        textureIndexPrime = Screen('MakeTexture', eyeTrackerWindow, imData); % define matrix as an image
        disp(['[LOAD ' num2str(iTrial) '] Prime image loaded']);
        
        %%% [LOAD] AUDIO
        % load stimuli
        disp(targetAudioPath);
        [targetAudio, freq] = audioread(targetAudioPath);
        waveDataGetter      = targetAudio';
        nChannels           = size(waveDataGetter, 1); % Number of rows == number of channels.
        PsychPortAudio('FillBuffer', pAudioHandle, waveDataGetter);
        disp(['[LOAD ' num2str(iTrial) '] Audio loaded']);

        %%% [LOAD] TARGET & DISTRACTOR
        if strcmp(targetLocation, 'r')
            rightImagePath           = targetImagePath;
            leftImagePath            = distractorImagePath;
            lookTextTargetDistractor = [char(words(ii, 3)) '-' char(words(ii, 2))];
        else
            rightImagePath           = distractorImagePath;
            leftImagePath            = targetImagePath;
            lookTextTargetDistractor = [char(words(ii, 2)) '-' char(words(ii, 3))];
        end
        
        % prepare image to the right
        imData        = imread(rightImagePath);
        [iy, ix, id]  = size(imData);
        iRectR        = [0 0 ix iy];                                      % resize the matrix
        textureIndexR = Screen('MakeTexture', eyeTrackerWindow, imData);
        %textureIndexR = Screen('MakeTexture', monitorWindow, imData);
        disp(['[LOAD ' num2str(iTrial) '] Right image (target/distractor) loaded']);

        % prepare image to the left
        imData        = imread(leftImagePath);
        [iy, ix, id]  = size(imData);
        iRectL        = [0 0 ix iy];
        textureIndexL = Screen('MakeTexture', eyeTrackerWindow, imData);
        %textureIndexL = Screen('MakeTexture', monitorWindow, imData);
        disp(['[LOAD ' num2str(iTrial) '] Left image (target/distractor) loaded']);
        disp(['[END ' num2str(iTrial) '] LOAD STIMULI']);

%         if (targetDistractorOrder)==1
%             AOI=iRectR;
%         else
%             AOI=iRectL;
%         end
        %WaitSecs('Untiltime',firstFlip + t3000);
        endGetter = toc(tIni);    
        
        %a1 = tic; % start counting until toc
        % [END] GETTERS        

        %% PRINT BACKGROUND SCREEN BOTH SCREENS
        Screen('FillRect', eyeTrackerWindow, BACKGROUND_COLOR);
        Screen('FillRect', monitorWindow, BACKGROUND_COLOR);
        Screen('Flip', eyeTrackerWindow);
        Screen('Flip', monitorWindow);  % display BACKGROUND_COLOR
        
        % prepare prime image
        Screen('DrawTexture', monitorWindow, textureIndexPrime, iRectPrime,...  % prepare the image to be presented
            [xCenter/2-(ix/4) yCenter/2-(iy/4) xCenter/2+(ix/4) yCenter/2+(iy/4)]);
        Screen('DrawTexture', eyeTrackerWindow, textureIndexPrime, iRectPrime,...  % prepare the image to be presented
            [xCenter-(ix/2) yCenter-(iy/2) xCenter+(ix/2) yCenter+(iy/2)]);
        
        % empty the eyeTracker buffer
        eyetracker.get_gaze_data('flat');
       
  
        %% [INI] PRIME IMAGE
        disp(['[INI ' num2str(iTrial) '] PRIME IMAGE']);
        
        % 2. prime image from 3000 to 4500 ms -----------------------------
        %fwrite(ser,'12'); %getter
        Screen('Flip', eyeTrackerWindow);
        Screen('Flip', monitorWindow);  % display BACKGROUND_COLOR
        tIni   = tic; 
        nFlips = 0;
        
        while toc(tIni)<=primeImageDuration
            
            %Screen('AsyncFlipEnd', monitorWindow);
            gazeData      = eyetracker.get_gaze_data('flat');
            %lookText     = strcat('LOOK_', char(words(ii, 1)));
            lookTextPrime = char(words(ii, 1));            
            %extra_cols=[participant,iTrial,num2str(num(ii,1)),words(ii,4),'Prime'];
            
            extra_cols = CONSTANTS.extra_cols;
            extra_cols.participant=participant;
            extra_cols.TrialNum=iTrial;
            extra_cols.Trial=num2str(num(ii,1));
            extra_cols.TargetLocation=words(ii,4);
            extra_cols.Phase='Prime';   
            
            
            %fullGazeData  = [fullGazeData; parseGazeData(gazeData, [participant,iTrial,num2str(num(ii,1)),words(ii,4),'Prime'])];
            fullGazeData = [fullGazeData; struct2table(gazeData)]; %SIL2020            
            full_extra_cols = [full_extra_cols; repmat(extra_cols, size(gazeData.device_time_stamp,1), 1)];%SIL2020   
            
            [x, y] = lastGazeData(gazeData); %x,y are from 0..1
            x      = x*screenXpixels; % scale to screen size
            y      = y*screenYpixels;
                         
            [ keyIsDown, ~, keyCode ] = KbCheck;
            if find(keyCode, 1) == E_KEY                
                throw(MException('Prime: warning', 'End by experimenter: inside Prime'))                
            end
            
            %MONITOR WINDOW            
            Screen('DrawTexture', monitorWindow, textureIndexPrime, iRectPrime,...  % prepare the image to be presented
                [xCenter/2-(ix/4) yCenter/2-(iy/4) xCenter/2+(ix/4) yCenter/2+(iy/4)]);
            %Screen('DrawDots', windowPtr, xy [,size] [,color] [,center] [,dot_type][, lenient]);
            Screen('DrawDots', monitorWindow, [x y]/SCREEN_MONITOR_PROPORTION, 40, EYE_DOT_COLOR, [], 2);
              
            %EYETRACKER WINDOW
            Screen('DrawTexture', eyeTrackerWindow, textureIndexPrime, iRectPrime,...  % prepare the image to be presented
                [xCenter-(ix/2) yCenter-(iy/2) xCenter+(ix/2) yCenter+(iy/2)]);
            if DRAW_CUES
                Screen('DrawDots', eyeTrackerWindow, [x y], 40, EYE_DOT_COLOR, [], 2);
            end
            
            %Screen('AsyncFlipBegin', monitorWindow);
            Screen('Flip', eyeTrackerWindow);
            Screen('Flip', monitorWindow); % display PRIME IMAGE
            nFlips = nFlips+1;
            
        end % WHILE TOC PRIME IMAGE       
       
%         size(fullGazeData,1)
%         size(fullGazeData_ALL,1)
%         fullGazeData_ALL = [fullGazeData_ALL; fullGazeData];
        
        fiPrime     = toc(tIni);
        nFlipsPrime = nFlips;
        nFlips      = 0;
        %fprintf(logFileID,'PRIME nFlips %d SECONDS %8.2f\t\n', nFlipsPrime,fiPrime);
        disp(['[END ' num2str(iTrial) '] PRIME IMAGE']);

        %% [INI] BLANK
        disp(['[INI ' num2str(iTrial) '] BLANK']);
        %fwrite(ser,'123'); %blank
        % 3. blank screen and audio from 4500 to 5700 ms (max) ------------
        % blank screen
               
        while toc(tIni)<=blankDuration            
            
            %Screen('AsyncFlipEnd', monitorWindow);
            gazeData      = eyetracker.get_gaze_data('flat');
            lookTextBlank = char(words(ii, 1));
            %fullGazeData = [fullGazeData; parseGazeData(gazeData, [language, condition, iTrial, "BLANK", lookText])];            
            %extra_cols=[participant,iTrial,num2str(num(ii,1)),words(ii,4),'Blank'];            
            
            extra_cols = CONSTANTS.extra_cols;
            extra_cols.participant=participant;
            extra_cols.TrialNum=iTrial;
            extra_cols.Trial=num2str(num(ii,1));
            extra_cols.TargetLocation=words(ii,4);
            extra_cols.Phase='Blank';
            
            %fullGazeData  = [fullGazeData; parseGazeData(gazeData, [participant,iTrial,num2str(num(ii,1)),words(ii,4),'Blank'])];
            fullGazeData = [fullGazeData; struct2table(gazeData)]; %SIL2020
            full_extra_cols = [full_extra_cols; repmat(extra_cols, size(gazeData.device_time_stamp,1), 1)];%SIL2020
            
            [x, y] = lastGazeData(gazeData); %x,y are from 0..1
            x = x*screenXpixels; % scale to screen size
            y = y*screenYpixels;
            
            [ keyIsDown, ~, keyCode ] = KbCheck;
            if find(keyCode, 1) == E_KEY
                throw(MException('Prime: warning', 'End by experimenter: inside Blank'))                
            end
            
            %MONITOR WINDOW BLANK
            Screen('FillRect', monitorWindow, BACKGROUND_COLOR);
            Screen('DrawDots', monitorWindow, [x y]/SCREEN_MONITOR_PROPORTION, 40, EYE_DOT_COLOR, [], 2);
                        
            %EYETRACKER WINDOW BLANK
            Screen('FillRect', eyeTrackerWindow, BACKGROUND_COLOR);
            if DRAW_CUES
                Screen('DrawDots', eyeTrackerWindow, [x y], 40, EYE_DOT_COLOR, [], 2);
            end
            %Screen('AsyncFlipBegin', monitorWindow);
            Screen('Flip', eyeTrackerWindow);
            Screen('Flip', monitorWindow);  
            nFlips = nFlips+1;
        disp(['[END ' num2str(iTrial) '] BLANK']);
        end %END WHILE BLANK
        
        %% [END] BLANK
        
        fiBlank     = toc(tIni);        
        nFlipsBlank = nFlips;
        nFlips      = 0;
        %fprintf(logFileID,'BLANK nFlips %d SECONDS %8.2f\t\n',nFlips,fiBlank);
        %fullGazeData_ALL = [fullGazeData_ALL; fullGazeData];
                
        %% [INI] AUDIO
        disp(['[INI ' num2str(iTrial) '] AUDIO']);
        targetAudioFlip = PsychPortAudio('Start', pAudioHandle, repetitions, 0, 1); % play audio
        startAudio      = toc(tIni);
        
        while toc(tIni)<=audioDuration
            
            %Screen('AsyncFlipEnd', monitorWindow);
            gazeData = eyetracker.get_gaze_data('flat');            
            %extra_cols=[participant,iTrial,num2str(num(ii,1)),words(ii,4),'Audio'];
            
            extra_cols = CONSTANTS.extra_cols;
            extra_cols.participant=participant;
            extra_cols.TrialNum=iTrial;
            extra_cols.Trial=num2str(num(ii,1));
            extra_cols.TargetLocation=words(ii,4);
            extra_cols.Phase='Audio';
            
            %fullGazeData  = [fullGazeData; parseGazeData(gazeData, [participant,iTrial,num2str(num(ii,1)),words(ii,4),'Audio'])];
            fullGazeData = [fullGazeData; struct2table(gazeData)]; %SIL2020
            full_extra_cols = [full_extra_cols; repmat(extra_cols, size(gazeData.device_time_stamp,1), 1)];%SIL2020
           
                       
            [x, y] = lastGazeData(gazeData); %x,y are from 0..1
            x      = x*screenXpixels; % scale to screen size
            y      = y*screenYpixels;                 
             
            [ keyIsDown, ~, keyCode ] = KbCheck;
            if find(keyCode, 1) == E_KEY
                throw(MException('Audio: Warning', 'End by experimenter: inside Audio'))                
            end
            
            % MONITOR WINDOW BACKGROUND
            Screen('FillRect', monitorWindow, BACKGROUND_COLOR);
            Screen('DrawDots', monitorWindow, [x y]/SCREEN_MONITOR_PROPORTION, 40, EYE_DOT_COLOR, [], 2);         
            
            % EYETRACKER WINDOW BACKGROUND
            Screen('FillRect', eyeTrackerWindow, BACKGROUND_COLOR);
            if DRAW_CUES
                Screen('DrawDots', eyeTrackerWindow, [x y], 40, EYE_DOT_COLOR, [], 2);
            end
            
            %Screen('AsyncFlipBegin', monitorWindow);
            Screen('Flip', eyeTrackerWindow);
            Screen('Flip', monitorWindow);
            nFlips = nFlips+1;
            
        end
        disp(['[END ' num2str(iTrial) '] AUDIO']);
        %% [END] AUDIO
        fiAudio     = toc(tIni);        
        nFlipsAudio = nFlips;
        fprintf(logFileID,'AUDIO nFlips %d SECONDS %8.2f\t\n',nFlips,fiAudio);
        nFlips      = 0;
        %fullGazeData_ALL = [fullGazeData_ALL; fullGazeData];
        
        %% [INI] TARGET & DISTRACTOR
        disp(['[INI ' num2str(iTrial) '] TARGET-DISTRACTOR']);
        % 5. target and distractor images from 5700 to 7700 ms ------------
        iniTargetDistractor = toc(tIni);
        lookTextTargetDistractor = [char(words(ii, 2)) '_' char(words(ii, 3))];
        while toc(tIni)<=totalDuration
            
            %Screen('AsyncFlipEnd', monitorWindow);
            gazeData = eyetracker.get_gaze_data('flat');
            %extra_cols=[participant,iTrial,num2str(num(ii,1)),words(ii,4),'Target-Distractor'];            
            
            extra_cols = CONSTANTS.extra_cols;
            extra_cols.participant=participant;
            extra_cols.TrialNum=iTrial;
            extra_cols.Trial=num2str(num(ii,1));
            extra_cols.TargetLocation=words(ii,4);
            extra_cols.Phase='Target-Distractor';
            
            %fullGazeData  = [fullGazeData; parseGazeData(gazeData, [participant,iTrial,num2str(num(ii,1)),words(ii,4),'Target-Distractor'])];
            fullGazeData = [fullGazeData; struct2table(gazeData)]; %SIL2020
            full_extra_cols = [full_extra_cols; repmat(extra_cols, size(gazeData.device_time_stamp,1), 1)];%SIL2020
            
            [x, y] = lastGazeData(gazeData); %x,y are from 0..1
            x = x*screenXpixels; % scale to screen size
            y = y*screenYpixels;
            
            [ keyIsDown, ~, keyCode ] = KbCheck;
            if find(keyCode, 1) == E_KEY
                throw(MException('PRIME_IMAGE: Warning', 'End by experimenter: inside TARGET-DISTRACTOR'))                
            end        
               
            Screen('DrawTexture', monitorWindow, textureIndexL, iRectL, [(xCenter/4)-(ix/4) yCenter/2-(iy/4) (xCenter/4)+(ix/4) yCenter/2+(iy/4)]);
            Screen('DrawTexture', monitorWindow, textureIndexR, iRectR, [xCenter/2+(xCenter/4)-(ix/4) yCenter/2-(iy/4) xCenter/2+(xCenter/4)+(ix/4) yCenter/2+(iy/4)]);
            Screen('DrawDots', monitorWindow, [x y]/SCREEN_MONITOR_PROPORTION, 40, EYE_DOT_COLOR, [], 2);
                        
            if strcmp(targetLocation, 'r')
                AOI=[xCenter/2+(xCenter/4)-(ix/4) yCenter/2-(iy/4) xCenter/2+(xCenter/4)+(ix/4) yCenter/2+(iy/4)];                
            else
                AOI=[(xCenter/4)-(ix/4) yCenter/2-(iy/4) (xCenter/4)+(ix/4) yCenter/2+(iy/4)];                
            end
            Screen('FrameRect', monitorWindow, [255 0 0], AOI, 5);
            
            %Screen('DrawTexture', eyeTrackerWindow, textureIndexL, iRectL, [(xCenter/2)-(ix/2)+50 yCenter-(iy/2) (xCenter/2)+(ix/2)+50 yCenter+(iy/2)]);
            %Screen('DrawTexture', eyeTrackerWindow, textureIndexR, iRectR, [xCenter+(xCenter/2)-(ix/2)-50 yCenter-(iy/2) xCenter+(xCenter/2)+(ix/2)-50 yCenter+(iy/2)]);
            Screen('DrawTexture', eyeTrackerWindow, textureIndexL, iRectL, [(xCenter/2)-(ix/2)-50 yCenter-(iy/2) (xCenter/2)+(ix/2)-50 yCenter+(iy/2)]); %20191125
            Screen('DrawTexture', eyeTrackerWindow, textureIndexR, iRectR, [xCenter+(xCenter/2)-(ix/2)+50 yCenter-(iy/2) xCenter+(xCenter/2)+(ix/2)+50 yCenter+(iy/2)]); %20191125
                       
            if DRAW_CUES
                Screen('DrawDots', eyeTrackerWindow, [x y], 40, EYE_DOT_COLOR, [], 2);
            end
            
            %Screen('AsyncFlipBegin', monitorWindow);
            Screen('Flip', eyeTrackerWindow);
            Screen('Flip',monitorWindow);
            nFlips = nFlips+1;
            
            %fullGazeData_ALL = [fullGazeData_ALL; fullGazeData];
            
        end
        
        fwrite(ser,'2');
        
        disp(['[END ' num2str(iTrial) '] TARGET-DISTRACTOR']);
        disp('[END] MAIN ROUTINE');
        %% FI TARGET & DISTRACTOR
        fiTargetDistractor     = toc(tIni);         
        nFlipsTargetDistractor = nFlips;
        nFlips                 = 0;
                
        %fullGazeData_ALL = [fullGazeData_ALL; fullGazeData];
        disp('[END] COGNATE PRIMING');
        
        %% [END] COGNATE PRIMING
        %fprintf(logDataPath,'\n');
        disp(['[TRIAL ' num2str(iTrial) ': BEFORE LOG]'])
        fprintf(logFileID,'TrialNum: %d TrialID: %d TrialTargetLocation %s Getter-FLIPS %d\t Getter-SEC %8.2f\t Prime-FLIPS %d\t Prime-SEC %8.2f\t Blank-FLIPS %d\t Blank-SEC %8.2f\t Audio-FLIPS %d\t Audio-SEC %8.2f\t TargetDistractor-FLIPS %d\t TargetDistractor-SEC %8.2f\t\n\n',...
            ii,targetLocation,nFlipsGetter,fiGetter,nFlipsPrime,fiPrime,nFlipsBlank,fiBlank,nFlipsAudio,fiAudio,nFlipsTargetDistractor,fiTargetDistractor);
        disp(['[TRIAL ' num2str(iTrial) ': AFTER LOG]'])
        
        fwrite(ser,'W');
        
    end % end of trial
    
    %% save data 
%     fullGazeData = cell2table(fullGazeData); 
%     fullGazeData.Properties.VariableNames = variableNames;
%     writetable(fullGazeData, gazeDataPath);
    table = [fullGazeData struct2table(full_extra_cols)];
    table.Properties.VariableNames = variableNames;
    writetable(table,gazeDataPath);       
    
    disp('[END] PRINT EYE-TRACKER DATA');

    eyetracker.stop_gaze_data();
    disp('[END] STOP EYE-TRACKER');

    PsychPortAudio('Stop', pAudioHandle);
    disp('[END] CLOSE AUDIO');
    
    %% [INI] CLEAR LIGHTS
    fwrite(ser, 'QWERTYUI'); %borrem abans de la cross
    fclose(ser); %close serial port
    delete(ser);
    clear ser;
    % [END] CLEAR LIGHTS
    disp('[END] CLOSE LIGHTS');

    
    %% [INI] CLEAR Psychotoolbox
    graphicsFinalize();
    audioFinalize(pAudioHandle);
    % [END] CLEAR Psychotoolbox
    eyeTrackerFinalize(eyetracker);
    disp('[END] CLOSE GRAPHICS, AUDIO AND EYE-TRACKER');
    
    %% [INI] Additional comments
    
    disp('[INI] Add comments');
    
    prompt2         = {'Additional comments'}; % define prompt variables
    promptDefaults2 = {comments};              % define prompt default values
    
    promptAnswer2 = inputdlg(prompt2, 'CognatePriming', 1, promptDefaults2); % define prompt properties
    [additionalComments] = deal(promptAnswer2{:}); % all variables are strings
    
    fprintf(logFileID, 'Comments: %s\n',additionalComments);
    
    disp('[END] Additional comments');
    
    Screen('CloseAll');
    fprintf(logFileID,'END: %s\s', datestr(now, 'dd/mm/yyyy HH:MM:SS'));
    fclose('all');
    
    disp('[TESTING FINISHED WITHOUT PROBLEMS]');

%% in case of error or forced exit
% do some stuff
catch ME
    if exist('iTrial', 'var')
        disp(['[END] Routine was interrupted due to error at trial ' num2str(iTrial)]);
    else
        disp('[END] Routine was interrupted due to error before starting');
    end         
    
    %% save data
    %     fullGazeData = cell2table(fullGazeData);
    %     fullGazeData.Properties.VariableNames = variableNames;
    %     writetable(fullGazeData, gazeDataPath);
    table = [fullGazeData   struct2table(full_extra_cols)];
    table.Properties.VariableNames = variableNames;
    writetable(table  , gazeDataPath);
    
    Screen('CloseAll');
    fprintf(logFileID,'END: %s\n', datestr(now, 'dd/mm/yyyy HH:MM:SS'));
    fclose('all');
end

if ~isempty(ME)
    rethrow(ME);
end