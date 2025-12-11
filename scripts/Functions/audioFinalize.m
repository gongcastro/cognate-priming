function audioFinalize(pahandle)
disp('[INI] Finalize Audio');
% Stop playback:
PsychPortAudio('Stop', pahandle);
% Close the audio device:
PsychPortAudio('Close', pahandle);
disp('[END] Finalize Audio');    
end