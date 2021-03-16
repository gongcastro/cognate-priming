function keyCode = waitKeyPress()
%waitKeyPress 
%   clear key buffer, waits for a pressKey and return the keyCode

% clear keyBuffer
while KbCheck
    KbReleaseWait;
end
while 1.
    [ keyIsDown, ~, keyCode ] = KbCheck;
    keyCode = find(keyCode, 1);
    if keyIsDown 
        KbReleaseWait;
        break;
    end
end

