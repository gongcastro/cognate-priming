function eyetracker = eyeTrackerInitialize()
%UNTITLED3 Summary of this function goes here
%   Detailed explanation goes here

%% [INI] Set up Tobii eye Tracker
disp('[INI] Set up eye Tracker');

try
    
disp('Set up eye Tracker: EyeTrackingOperations()');
Tobii = EyeTrackingOperations();

%% Find All EyeTrackers
disp('Set up eye Tracker: Tobii.find_all_eyetrackers()');
eyetrackers = Tobii.find_all_eyetrackers();

for i=1:size(eyetrackers,2)
    disp(['Address:',eyetrackers(i).Address])
    disp(['Name:',eyetrackers(i).Name])
    disp(['Serial Number:',eyetrackers(i).SerialNumber])
    disp(['Model:',eyetrackers(i).Model])
    disp(['Firmware Version:',eyetrackers(i).FirmwareVersion])
    disp(['Runtime Version:',eyetrackers(i).RuntimeVersion])
    f=eyetrackers(i).get_all_gaze_output_frequencies();
    aviable_freq='';
    for i=1:size(f,1)
        aviable_freq=[aviable_freq,' ',num2str(f(i))];
    end
    disp(['Aviable freqs: ', aviable_freq]);
    fprintf('\n')
end

if (size(eyetrackers,2)==0)
    disp('ERROR: no Eyetrackers Found');
    exit;
end

if (size(eyetrackers,2)>1)
    disp('ALERT: more than one eyetracker found');
    disp(' We will use the first one');
end

% Select the eyetracker to work with
eyetracker = Tobii.get_eyetracker(eyetrackers(1).Address);
eyetracker.set_gaze_output_frequency(120);
%eyetracker.set_gaze_output_frequency(300);

catch ME
    disp("ERROR trying to connect eyeTracker!!!!!");
    disp(ME);
end
disp('[END] Set up eye Tracker');


end

