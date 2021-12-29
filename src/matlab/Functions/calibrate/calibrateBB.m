function calibrateBB(eyetracker, window, monitorWindow, pAudioHandle, filenamecalibration)
%     global SCREEN_MONITOR_PROPORTION;
%     global SCREEN_WIDTH;
%     global SCREEN_HEIGHT;
%     global TEXT_COLOR;
%     global BACKGROUND_COLOR; %20200203 SIL as chiara
    
    TEXT_COLOR = [255, 255, 255]; %20200203 SIL as chiara
    
    [SCREEN_WIDTH, SCREEN_HEIGHT] = Screen('WindowSize', window); %20200203 SIL as chiara
    [mScreenXpixels, mScreenYpixels] = Screen('WindowSize', monitorWindow); %20200203 SIL as chiara
    SCREEN_MONITOR_PROPORTION = SCREEN_WIDTH / mScreenXpixels; %20200203 SIL as chiara

    spaceKey = KbName('Space');
    RKey = KbName('R');
        
    % image to attract atention
    % Import image and and convert it, stored in
    % MATLAB matrix, into a Psychtoolbox OpenGL texture using 'MakeTexture';
    imageFileName = ["spiral0.png","spiral0.png","spiral0.png","spiral0.png"];
    audioFileName = ["Ring01.wav","Ring02.wav","Ring03.wav","Ring04.wav"];

    % Import image and and convert it, stored in
    % MATLAB matrix, into a Psychtoolbox OpenGL texture using 'MakeTexture';
    for stimIndex = 1:size(imageFileName,2)
        [img, ~, alpha]=imread(imageFileName(stimIndex), 'png');
        [iy, ix, ~] = size(img);
        img(:, :, 4) = alpha;
        STIM(stimIndex).texture = Screen('MakeTexture', window, img);
        STIM(stimIndex).iRect = [0 0 ix iy];
        STIM(stimIndex).x = 100;
        STIM(stimIndex).y = 100;
        STIM(stimIndex).AOI = CenterRectOnPoint(STIM(stimIndex).iRect/2, STIM(stimIndex).x, STIM(stimIndex).y); % stimolous Area Of Interest (AOI)
        STIM(stimIndex).steepX = 10;
        STIM(stimIndex).steepY = 10;
        STIM(stimIndex).rotation = 5;
        STIM(stimIndex).rotationSteep = 5;
        STIM(stimIndex).zoom = 1;
        
        [waveData, ~] = audioread(audioFileName(stimIndex));
        STIM(stimIndex).audioData = [waveData waveData]';
        STIM(stimIndex).audioFileName = audioFileName;
    end
    
    
    dotSizePix = 30;
    
    % Get the size of the on screen window in pixels.
    % For help see: Screen WindowSize?
    [screenXpixels, screenYpixels] = Screen('WindowSize', window);
    screen_pixels = [screenXpixels screenYpixels];
    
    dotColor = [[1 0 0];[1 1 1]]; % Red and white

    leftColor = [1 0 0]; % Red
    rightColor = [0 0 1]; % Bluesss

    % Calibration points
    lb = 0.1;  % left bound
    xc = 0.5;  % horizontal center
    rb = 0.9;  % right bound
    ub = 0.1;  % upper bound
    yc = 0.5;  % vertical center
    bb = 0.9;  % bottom bound

    points_to_calibrate = [[lb,ub];[rb,ub];[xc,yc];[lb,bb];[rb,bb]];

    % Create calibration object
    calib = ScreenBasedCalibration(eyetracker);
    try
        calib.leave_calibration_mode();
    catch 
    end

    calibrating = true;
    


    while calibrating
        % Enter calibration mode
        calib.enter_calibration_mode();

%         stimIndex=randi(size(imageFileName,2));
%         PsychPortAudio('FillBuffer', pAudioHandle, STIM(stimIndex).audioData);
        
        for i=1:length(points_to_calibrate)
            %stimIndex=mod(i, size(imageFileName,2))+1;
            stimIndex=randi(size(imageFileName,2));
            PsychPortAudio('FillBuffer', pAudioHandle, STIM(stimIndex).audioData);
            PsychPortAudio('Start', pAudioHandle);

            for j=60:-1:0 
                %Screen('DrawDots', window, points_to_calibrate(i,:).*screen_pixels, dotSizePix, dotColor(1,:), [], 2);
                %Screen('DrawDots', window, points_to_calibrate(i,:).*screen_pixels, dotSizePix*0.5, dotColor(2,:), [], 2);
                point = points_to_calibrate(i,:).*screen_pixels;
                STIM(stimIndex).rotation = mod(STIM(stimIndex).rotation + STIM(stimIndex).rotationSteep, 360);
                STIM(stimIndex).zoom = 0.1+j/60;
                STIM(stimIndex).centerRect = CenterRectOnPoint(STIM(stimIndex).iRect*STIM(stimIndex).zoom, point(1), point(2));
                
                %Screen('DrawTexture', windowPointer, texturePointer [,sourceRect] [,destinationRect] [,rotationAngle] [, filterMode] [, globalAlpha] [, modulateColor] [, textureShader] [, specialFlags] [, auxParameters]);
                Screen('DrawTexture', window, STIM(stimIndex).texture, STIM(stimIndex).iRect, STIM(stimIndex).centerRect, STIM(stimIndex).rotation);
                Screen('Flip', window);
                Screen('DrawTexture', monitorWindow, STIM(stimIndex).texture, STIM(stimIndex).iRect, STIM(stimIndex).centerRect/SCREEN_MONITOR_PROPORTION, STIM(stimIndex).rotation);
                Screen('Flip', monitorWindow);
                
                % Wait a moment to allow the user to focus on the point
                pause(0.01);
            end
            
            calib.collect_data(points_to_calibrate(i,:));

% %             if calib.collect_data(points_to_calibrate(i,:)) ~= CalibrationStatus.Success
% %                 disp("[ini]calib.collect_data");
% %                 % Try again if it didn't go well the first time.
% %                 % Not all eye tracker models will fail at this point, but instead fail on ComputeAndApply.
% %                 calib.collect_data(points_to_calibrate(i,:));
% %                 disp("[end]calib.collect_data");
% %             end
            
            
        end %for i=1:length(points_to_calibrate)

        [nx, ny, textbounds, wordbounds] = DrawFormattedText(window, 'Calculating calibration result....', 'center', 'center', TEXT_COLOR);
        Screen('Flip', window);
        DrawFormattedText(monitorWindow, 'Calculating calibration result....', textbounds(1)/SCREEN_MONITOR_PROPORTION, textbounds(2)/SCREEN_MONITOR_PROPORTION, TEXT_COLOR);
        Screen('Flip', monitorWindow);
        
        % Blocking call that returns the calibration result
        calibration_result = calib.compute_and_apply();

        calib.leave_calibration_mode();

        if calibration_result.Status ~= CalibrationStatus.Success
            %break
        end

        % Calibration Result
        points = calibration_result.CalibrationPoints;
        if ~isempty(points)
            for i=1:length(points)
                Screen('DrawDots', window, points(i).PositionOnDisplayArea.*screen_pixels, dotSizePix*0.5, dotColor(2,:), [], 2);
                Screen('DrawDots', monitorWindow, points(i).PositionOnDisplayArea.*screen_pixels/SCREEN_MONITOR_PROPORTION, dotSizePix*0.5, dotColor(2,:), [], 2);
                
                for j=1:length(points(i).RightEye)
                    if points(i).LeftEye(j).Validity == CalibrationEyeValidity.ValidAndUsed
                        Screen('DrawDots', window, points(i).LeftEye(j).PositionOnDisplayArea.*screen_pixels, dotSizePix*0.3, leftColor, [], 2);
                        Screen('DrawLines', window, ([points(i).LeftEye(j).PositionOnDisplayArea; points(i).PositionOnDisplayArea].*screen_pixels)', 2, leftColor, [0 0], 2);
                        Screen('DrawDots', monitorWindow, points(i).LeftEye(j).PositionOnDisplayArea.*screen_pixels/SCREEN_MONITOR_PROPORTION, dotSizePix*0.3, leftColor, [], 2);
                        Screen('DrawLines', monitorWindow, ([points(i).LeftEye(j).PositionOnDisplayArea; points(i).PositionOnDisplayArea].*screen_pixels)'/SCREEN_MONITOR_PROPORTION, 2, leftColor, [0 0], 2);
                        
                    end
                    if points(i).RightEye(j).Validity == CalibrationEyeValidity.ValidAndUsed
                        Screen('DrawDots', window, points(i).RightEye(j).PositionOnDisplayArea.*screen_pixels, dotSizePix*0.3, rightColor, [], 2);
                        Screen('DrawLines', window, ([points(i).RightEye(j).PositionOnDisplayArea; points(i).PositionOnDisplayArea].*screen_pixels)', 2, rightColor, [0 0], 2);
                        Screen('DrawDots', monitorWindow, points(i).RightEye(j).PositionOnDisplayArea.*screen_pixels/SCREEN_MONITOR_PROPORTION, dotSizePix*0.3, rightColor, [], 2);
                        Screen('DrawLines', monitorWindow, ([points(i).RightEye(j).PositionOnDisplayArea; points(i).PositionOnDisplayArea].*screen_pixels)'/SCREEN_MONITOR_PROPORTION, 2, rightColor, [0 0], 2);
                        
                    end
                end
            end
        else
            %DrawFormattedText(window, 'No points calibrated, please, repeat calibration....', 'center', 'center', TEXT_COLOR);
            DrawFormattedText(monitorWindow, 'No points calibrated, please, repeat calibration....', 'center', 'center', TEXT_COLOR);
           
        end
        
        %save([filenamecalibration '.mat'],points); %20200203 SIL
        %save(points);
        
        %Screen('FillRect', window, BACKGROUND_COLOR); % fill the window with grey colour
        DrawFormattedText(window, 'Press the ''R'' key to recalibrate or ''Space'' to continue....', 'center', screenYpixels * 0.95/SCREEN_MONITOR_PROPORTION, TEXT_COLOR);
        Screen('Flip', window);               
        
        DrawFormattedText(monitorWindow, 'Press the ''R'' key to recalibrate or ''Space'' to continue....', 'center', screenYpixels * 0.95/SCREEN_MONITOR_PROPORTION, TEXT_COLOR);
        Screen('Flip', monitorWindow);
        imageArray=Screen('GetImage',window);
        filenamecalibration = [filenamecalibration(1:(length(filenamecalibration)-4)) '_CALIB.jpg'];
        imwrite(imageArray,filenamecalibration);               
        
        while 1.
            [ keyIsDown, ~, keyCode ] = KbCheck;
            keyCode = find(keyCode, 1);

            if keyIsDown
                if keyCode == spaceKey
                    calibrating = false;
                    break;
                elseif keyCode == RKey
                    break;
                end
                KbReleaseWait;
            end
        end
    end %while calibrating
   
end    