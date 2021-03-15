function [ret] = inrect(point, rect)
%INRECT Summary of this function goes here
%   Detailed explanation goes here

    ret = point(1)>rect(1) && point(1)<rect(3) && point(2)>rect(2) && point(2)<rect(4);

end

