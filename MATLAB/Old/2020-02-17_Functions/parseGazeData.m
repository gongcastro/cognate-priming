function [data] = parseGazeData(gaze_data, extra_cols)
%UNTITLED parse gaza data 
%INPUT
% gaze_data in Tobii pro SDK format
%OUTPUT
% matrix array on each colum is a simple value
% SystemTime meanX meanY meanValidity lX lY lV lPupil lPupilV rX rY rV rPupil rPupilV
% % 

% preallocate memory for performance
%tmp_data = single(zeros(size(gaze_data,1), 26));
tmp_data = double(zeros(size(gaze_data,1), 26));
for i=1: size(gaze_data,1)
    % select the eyeTracker values that we need
    tmp_data(i,:) = [...
        
    %double(gaze_data(i).SystemTimeStamp)
    %double(gaze_data(i).DeviceTimeStamp)
    double(gaze_data(i).SystemTimeStamp)
    double(gaze_data(i).DeviceTimeStamp)
    double(gaze_data(i).LeftEye.GazePoint.OnDisplayArea(1))
    double(gaze_data(i).LeftEye.GazePoint.OnDisplayArea(2))
    double(gaze_data(i).LeftEye.GazePoint.Validity.value)
    (gaze_data(i).LeftEye.Pupil.Diameter)
    double(gaze_data(i).LeftEye.Pupil.Validity.value)
    double(gaze_data(i).LeftEye.GazeOrigin.InUserCoordinateSystem(1))
    double(gaze_data(i).LeftEye.GazeOrigin.InUserCoordinateSystem(2))
    double(gaze_data(i).LeftEye.GazeOrigin.InUserCoordinateSystem(3))
    double(gaze_data(i).LeftEye.GazeOrigin.InTrackBoxCoordinateSystem(1))
    double(gaze_data(i).LeftEye.GazeOrigin.InTrackBoxCoordinateSystem(2))
    double(gaze_data(i).LeftEye.GazeOrigin.InTrackBoxCoordinateSystem(3))
    double(gaze_data(i).LeftEye.GazeOrigin.Validity.value)
    double(gaze_data(i).RightEye.GazePoint.OnDisplayArea(1))
    double(gaze_data(i).RightEye.GazePoint.OnDisplayArea(2))
    double(gaze_data(i).RightEye.GazePoint.Validity.value)
    double(gaze_data(i).RightEye.Pupil.Diameter)
    double(gaze_data(i).RightEye.Pupil.Validity.value)
    double(gaze_data(i).RightEye.GazeOrigin.InUserCoordinateSystem(1))
    double(gaze_data(i).RightEye.GazeOrigin.InUserCoordinateSystem(2))
    double(gaze_data(i).RightEye.GazeOrigin.InUserCoordinateSystem(3))
    double(gaze_data(i).RightEye.GazeOrigin.InTrackBoxCoordinateSystem(1))
    double(gaze_data(i).RightEye.GazeOrigin.InTrackBoxCoordinateSystem(2))
    double(gaze_data(i).RightEye.GazeOrigin.InTrackBoxCoordinateSystem(3))
    double(gaze_data(i).RightEye.GazeOrigin.Validity.value)
    
    ];
end

% generate the same number of row for extra-cols
t = repmat(extra_cols, size(gaze_data,1), 1);

% add extra cols to eyeTraker data
%data = table(tmp_data, t);
data = [num2cell(tmp_data) t];


% % select the values that you need
%single(gaze_data(i).SystemTimeStamp)/1000000
%single(gaze_data(i).DeviceTimeStamp)/1000000
%gaze_data(i).LeftEye.GazePoint.OnDisplayArea(1) 
%gaze_data(i).LeftEye.GazePoint.OnDisplayArea(2) 
%gaze_data(i).LeftEye.GazePoint.Validity.value 
%gaze_data(i).LeftEye.Pupil.Diameter
%gaze_data(i).LeftEye.Pupil.Validity.value
%gaze_data(i).RightEye.GazePoint.OnDisplayArea(1) 
%gaze_data(i).RightEye.GazePoint.OnDisplayArea(2) 
%gaze_data(i).RightEye.GazePoint.Validity.value 
%gaze_data(i).RightEye.Pupil.Diameter
%gaze_data(i).RightEye.Pupil.Validity.value
%gaze_data(i).RightEye.GazeOrigin.InUserCoordinateSystem(1)
%gaze_data(i).RightEye.GazeOrigin.InUserCoordinateSystem(2)
%gaze_data(i).RightEye.GazeOrigin.InUserCoordinateSystem(3)
%gaze_data(i).RightEye.GazeOrigin.InTrackBoxCoordinateSystem(1)
%gaze_data(i).RightEye.GazeOrigin.InTrackBoxCoordinateSystem(2)
%gaze_data(i).RightEye.GazeOrigin.InTrackBoxCoordinateSystem(3)
%gaze_data(i).RightEye.GazeOrigin.Validity.value
end

