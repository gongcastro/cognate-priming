%% generate an elipse
% theta = 0;   angle that will be increased each loop
% h = 12      // x coordinate of circle center
% k = 10      // y coordinate of circle center
% step = 15;  // amount to add to theta each time (degrees)
% 
% repeat until theta >= 360;
%     { x = h + r*cos(theta)
%       y = k + r*sin(theta)
%       draw a line to x,y
%       add step to theta
%     }


% radius=3;%just an example
% theta=linspace(0,2*pi,200)%you can increase this if this isn't enough yet
% x=radius*cos(theta);
% y=radius*sin(theta);

% Initialize graphics mode


theta=linspace(0,2*pi,400);
points = zeros(size(theta,2), 2);
centerX = 1920/2;
centerY = 1080/2;
radiusX=centerX-100;
radiusY=centerY-100;

for i=1: size(theta,2)
    points(i,1) = (rand()*50)+centerX+radiusX*cos(theta(i));
    points(i,2) = (rand()*50)+centerY+radiusY*sin(theta(i));
end
    

Screen('Preference', 'SkipSyncTests', 1);
[windowPtr, windowRect] = Screen('OpenWindow', 2);

for i=1:400
    rect = [points(i,1)-5, points(i,2)-5, points(i,1)+5, points(i,2)+5];
    Screen('FillOval', windowPtr ,[255 0 0], rect);
    
end
Screen('Flip', windowPtr);

    
waitKeyPress();
Screen('CloseAll');


