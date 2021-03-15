% all operating systems:
KbName('UnifyKeyNames');


try

    % Removes the blue screen flash and minimize extraneous warnings.
	Screen('Preference', 'VisualDebugLevel', 3);
    Screen('Preference', 'SuppressAllWarnings', 1);
    
    % Hide the mouse cursor.
    %HideCursor;
	
    % Find out how many screens and use largest screen number.
    whichScreen = max(Screen('Screens'));
    
    % Open a new window.
    [ window, windowRect ] = Screen('OpenWindow', whichScreen, [0 0 0], [100 100 600 600] );
    %  [ window, windowRect ] = Screen('OpenWindow', whichScreen, [0 0 0]);
    
    % Set text display options. We skip on Linux.
    if ~IsLinux
        Screen('TextFont', window, 'Arial');
        Screen('TextSize', window, 18);
    end

    % Set colors.
    black = BlackIndex(window);
    rotationAngle = 0;
    rotationStep = -5;
    sizeStep = 5;
    sizeMin = 25; % indicates percentge
    sizeNew = 0;
    
 
    
    
    
    
    % Import image and and convert it, stored in
    % MATLAB matrix, into a Psychtoolbox OpenGL texture using 'MakeTexture';
    myimgfile = 'image5.png'; 
    fprintf('Using image ''%s''\n', myimgfile);
    imdata=imread(myimgfile, 'png');
    
    % Crop image if it is larger then screen size. There's no image scaling
    % in maketexture
    [iy, ix, id]=size(imdata);
    iRect = [0 0 ix iy];
    
    
    [wW, wH]=WindowSize(window);
    wRect = [round(wW/2-ix/2) round(wH/2-iy/2) round(wW/2-ix/2)+ix round(wH/2-iy/2)+iy];
    sizeNew = wW;
    sizeMin = wW * sizeMin/100;
    
    % Set up texture and rects
    imagetex=Screen('MakeTexture', window, imdata(1:iy, 1:ix,:));
    Screen('DrawTexture', window, imagetex, iRect, wRect, rotationAngle);
    
    
    Screen('Flip', window);
    
    color = 0;
    while ~KbCheck; 
        
        color = color + 1;
        %Flor(window, color);
        %Spiral1(window, color);
        % Spiral2(window, color);
        
        rotationAngle = mod(rotationAngle - rotationStep, 360);
        
        sizeNew = sizeNew - sizeStep
        if wW<sizeNew || sizeNew<sizeMin
           sizeStep =  sizeStep * -1;
           rotationStep = rotationStep * -1;
        end
        
        
        w = sizeNew;
        h = sizeNew;
        
        wRect = [round(wW/2-w/2) round(wH/2-h/2) round(wW/2-w/2)+w round(wH/2-h/2)+h];
        Screen('DrawTexture', window, imagetex, iRect, wRect, rotationAngle);
        Screen('Flip', window);
        %WaitSecs(0.01);
    end
    
    
    % Hide the mouse cursor.
    ShowCursor;
    Screen('CloseAll');
catch
    ShowCursor;
    Screen('CloseAll');
    psychrethrow(psychlasterror);
end