function [x, y] = lastGazeData(gaze_data)
%UNTITLED parse gaza data 
%INPUT
% gaze_data in Tobii pro SDK format
%OUTPUT
% matrix array on each colum is a simple value
% % 
x=0;
y=0;
if ~isempty(gaze_data)
    last_gaze = gaze_data(end);
    if last_gaze.LeftEye.GazePoint.Validity.Valid && last_gaze.RightEye.GazePoint.Validity.Valid
        eye_color = [0 255 0];
    else
        eye_color = [255 0 0];
    end
    l_x = double(last_gaze.LeftEye.GazePoint.OnDisplayArea(1));
    l_y = double(last_gaze.LeftEye.GazePoint.OnDisplayArea(2));
    r_x = double(last_gaze.RightEye.GazePoint.OnDisplayArea(1));
    r_y = double(last_gaze.RightEye.GazePoint.OnDisplayArea(2));
    x = mean([l_x r_x]);
    y = mean([l_y r_y]);
end

