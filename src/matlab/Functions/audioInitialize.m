function pahandle = audioInitialize()
disp('[INI] Initialize Audio');    
    PsychPortAudio('Verbosity', 5); % 0 = Shut up, 1 = Print errors,...5 = Be very verbose

    % Perform basic initialization of the sound driver:
    reallyneedlowlatency=0;
    %'reallyneedlowlatency = 1' to push really hard for low latency
    %'reallyneedlowlatency = 0' not to push really hard for low latency
    InitializePsychSound(reallyneedlowlatency);


    %search for Low Latency drivers/devices
    devicetype = []; % to get all de aviable devices
    deviceIndex = []; % to get all de aviable devices
    devices = PsychPortAudio('GetDevices' , devicetype, deviceIndex);

    selectedDevice = 0;
    deviceIndex = 0;
    LowOutputLatency = Inf;
    for i=1:length(devices)
        if (devices(i).NrOutputChannels>0) % looking for output
            if (devices(i).LowOutputLatency<LowOutputLatency && devices(i).LowOutputLatency>0.0)
                LowOutputLatency = devices(i).LowOutputLatency;
                deviceIndex = devices(i).DeviceIndex;
                selectedDevice = i;
            end
        end
    end

    selectedDevice = 3;
    fprintf ('\n\nDevice Name: %s, \tDevice index: %i, \tLowOutputLatency: %3.3fms\n\n', devices(selectedDevice).DeviceName, devices(selectedDevice).DeviceIndex, devices(selectedDevice).LowOutputLatency);

    deviceid = devices(selectedDevice).DeviceIndex;
    mode = 1; % 1 == sound playback only
    reqlatencyclass = 2; %  Level 2 means: Take full control over the audio device
    %freq = []; % Defaults to a value that depends on the requested latency mode.
    freq = 44100; % Defaults to a value that depends on the requested latency mode.
    channels = 2; % supose stero files
    buffersize = 0; % Pointless to set this. Auto-selected to be optimal.
    suggestedLatency = []; %  Best left alone, only here as manual override in case all the auto-tuning cleverness fails.
    selectchannels = [1, 2]; % frist 2 output channels
    specialFlags = 0;
    try 
        % pahandle = PsychPortAudio('Open', deviceid, mode, reqlatencyclass, freq, channels, buffersize, suggestedLatency, selectchannels, specialFlags);
        % pahandle = PsychPortAudio('Open', deviceid, mode, reqlatencyclass, freq, channels);%, buffersize, suggestedLatency, selectchannels, specialFlags);
        pahandle = PsychPortAudio('Open', deviceid, [], 0, [], channels);
    catch
        ME = MException('PsychPortAudio:Open', ...
             'Problem open audio device, try to close devices e.x: "close all" ');
        throw(ME);
    end

    fprintf('Device latency: %3.3f\n', PsychPortAudio('LatencyBias', pahandle));
disp('[END] Initialize Audio');        
end