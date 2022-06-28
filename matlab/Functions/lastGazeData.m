function [x, y] = lastGazeData(gaze_data)
%UNTITLED parse gaza data 
%INPUT
% gaze_data in Tobii pro SDK format
%OUTPUT
% matrix array on each colum is a simple value
x=0;
y=0;
if ~isempty(gaze_data.device_time_stamp)
    x = mean([double(gaze_data.left_gaze_point_on_display_area(:,1)) ; double(gaze_data.right_gaze_point_on_display_area(:,1))]);
    y = mean([double(gaze_data.left_gaze_point_on_display_area(:,2)) ; double(gaze_data.right_gaze_point_on_display_area(:,2))]);
end
