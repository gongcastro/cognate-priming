function trackgaze(eyetracker, window)
%TRACKGAZE Summary of this function goes here
%   Detailed explanation goes here
    dotSizePix = 30;
    % Get the size of the on screen window in pixels.
    % For help see: Screen WindowSize?
    [screenXpixels, screenYpixels] = Screen('WindowSize', window);
    screen_pixels = [screenXpixels screenYpixels];
    pointCount = 5;

    % Generate an array with coordinates of random points on the display area
    rng;
    points = rand(pointCount,2);

    % Start to collect data
    eyetracker.get_gaze_data();

    collection_time_s = 2; % seconds

    for i=1:pointCount
        tIni=tic;
        while toc(tIni)<collection_time_s
            %Screen('DrawDots', window, points(i,:).*screen_pixels, dotSizePix, [255 255 255], [], 2);
            Screen('FillRect', window, [255 255 255], [points(i,:).*screen_pixels points(i,:).*screen_pixels+100]);
            gaze_data = eyetracker.get_gaze_data();
            if ~isempty(gaze_data)
                last_gaze = gaze_data(end);
                if last_gaze.LeftEye.GazePoint.Validity.Valid && last_gaze.RightEye.GazePoint.Validity.Valid
                    eye_color =[0 255 0];
                else
                    eye_color = [255 0 0];
                end
                l_x = double(last_gaze.LeftEye.GazePoint.OnDisplayArea(1))*screenXpixels;
                l_y = double(last_gaze.LeftEye.GazePoint.OnDisplayArea(2))*screenYpixels;
                r_x = double(last_gaze.RightEye.GazePoint.OnDisplayArea(1))*screenXpixels;
                r_y = double(last_gaze.RightEye.GazePoint.OnDisplayArea(2))*screenYpixels;
                x = mean([l_x r_x]);
                y = mean([l_y r_y]);
                if inrect([x y], [points(i,:).*screen_pixels points(i,:).*screen_pixels+100])
                    Screen('FrameRect', window, [255 0 0], [points(i,:).*screen_pixels points(i,:).*screen_pixels+100],5);
                end
                Screen('DrawDots', window, [x y], dotSizePix, eye_color, [], 2);

                Screen('Flip', window);
            end
        end
    end


    eyetracker.stop_gaze_data();

end

