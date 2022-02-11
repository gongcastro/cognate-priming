function position(eyetracker, window, monitorWindow)
%POSITION Summary of this function goes here
%   Detailed explanation goes here
    global SCREEN_MONITOR_PROPORTION;

    TEXT_COLOR = [255 0 0];
    % Dot size in pixels
    dotSizePix = 30;
    [screenXpixels, screenYpixels] = Screen('WindowSize', window);
    [xCenter, yCenter] = RectCenter(Screen('Rect', window));

    % Start collecting data
    % The subsequent calls return the current values in the stream buffer.
    % If a flat structure is prefered just use an extra input 'flat'.
    % i.e. gaze_data = eyetracker.get_gaze_data('flat');
    eyetracker.get_gaze_data();

    

    while ~KbCheck
        %Screen('FillRect', window, BACKGROUND_COLOR);
        [nx, ny, textbounds, wordbounds]=DrawFormattedText(window, 'When correctly positioned press any key to start the calibration.', 'center', screenYpixels * 0.1, [0 0 0]);
        Screen('AsyncFlipEnd', monitorWindow);
        DrawFormattedText(monitorWindow, 'When correctly positioned press any key to start the calibration.', textbounds(1)/SCREEN_MONITOR_PROPORTION, textbounds(2)/SCREEN_MONITOR_PROPORTION, [0 0 0]);

        distance = [];

        gaze_data = eyetracker.get_gaze_data();

        if ~isempty(gaze_data)
            last_gaze = gaze_data(end);

            validityColor = [255 0 0];

            % Check if user has both eyes inside a reasonable tacking area.
            if last_gaze.LeftEye.GazeOrigin.Validity.Valid && last_gaze.RightEye.GazeOrigin.Validity.Valid
                left_validity = all(last_gaze.LeftEye.GazeOrigin.InTrackBoxCoordinateSystem(1:2) < 0.85) ...
                                     && all(last_gaze.LeftEye.GazeOrigin.InTrackBoxCoordinateSystem(1:2) > 0.15);
                right_validity = all(last_gaze.RightEye.GazeOrigin.InTrackBoxCoordinateSystem(1:2) < 0.85) ...
                                     && all(last_gaze.RightEye.GazeOrigin.InTrackBoxCoordinateSystem(1:2) > 0.15);
                if left_validity && right_validity
                    validityColor = [0 255 0];
                end
            end

            origin = [screenXpixels/4 screenYpixels/4];
            size = [screenXpixels/2 screenYpixels/2];

            penWidthPixels = 3;
            baseRect = [0 0 size(1) size(2)];
            frame = CenterRectOnPointd(baseRect, screenXpixels/2, yCenter);

            Screen('FrameRect', window, validityColor, frame, penWidthPixels);
            Screen('FrameRect', monitorWindow, validityColor, frame/SCREEN_MONITOR_PROPORTION, penWidthPixels);

            % Left Eye
            if last_gaze.LeftEye.GazeOrigin.Validity.Valid
                distance = [distance; round(last_gaze.LeftEye.GazeOrigin.InUserCoordinateSystem(3)/10,1)];
                left_eye_pos_x = double(1-last_gaze.LeftEye.GazeOrigin.InTrackBoxCoordinateSystem(1))*size(1) + origin(1);
                left_eye_pos_y = double(last_gaze.LeftEye.GazeOrigin.InTrackBoxCoordinateSystem(2))*size(2) + origin(2);
                Screen('DrawDots', window, [left_eye_pos_x left_eye_pos_y], dotSizePix, validityColor, [], 2);
                Screen('DrawDots', monitorWindow, [left_eye_pos_x left_eye_pos_y]/SCREEN_MONITOR_PROPORTION, dotSizePix, validityColor, [], 2);
            end

            % Right Eye
            if last_gaze.RightEye.GazeOrigin.Validity.Valid
                distance = [distance;round(last_gaze.RightEye.GazeOrigin.InUserCoordinateSystem(3)/10,1)];
                right_eye_pos_x = double(1-last_gaze.RightEye.GazeOrigin.InTrackBoxCoordinateSystem(1))*size(1) + origin(1);
                right_eye_pos_y = double(last_gaze.RightEye.GazeOrigin.InTrackBoxCoordinateSystem(2))*size(2) + origin(2);
                Screen('DrawDots', window, [right_eye_pos_x right_eye_pos_y], dotSizePix, validityColor, [], 2);
                Screen('DrawDots', monitorWindow, [right_eye_pos_x right_eye_pos_y]/SCREEN_MONITOR_PROPORTION, dotSizePix, validityColor, [], 2);
            end
            pause(0.05); 
        end

        distanceColor = [255 0 0];
        if (60.0<mean(distance)) && (mean(distance)<70.0)
            distanceColor = [0 255 0];
        end
        [nx, ny, textbounds, wordbounds] = DrawFormattedText(window, sprintf('Current distance to the eye tracker(optimal 65cm): %.2f cm.',mean(distance)), 'center', screenYpixels * 0.85, [0 0 0]);
        Screen('DrawDots', window, [nx+dotSizePix ny-dotSizePix/2], dotSizePix, distanceColor, [], 2);
        [nx, ny, textbounds, wordbounds] = DrawFormattedText(monitorWindow, sprintf('Current distance to the eye tracker(optimal 65cm): %.2f cm.',mean(distance)), textbounds(1)/SCREEN_MONITOR_PROPORTION, textbounds(2)/SCREEN_MONITOR_PROPORTION, [0 0 0]);
        Screen('DrawDots', monitorWindow, [nx+dotSizePix ny-dotSizePix/2], dotSizePix, distanceColor, [], 2);

        Screen('AsyncFlipBegin', monitorWindow);
        Screen('Flip', window);
        

    end

    eyetracker.stop_gaze_data();
    Screen('AsyncFlipEnd', monitorWindow);

end

