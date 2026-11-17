clc,clear;

speciesList = xlsread('.\Data\SpeciesList.xlsx',1);

fileNames = dir(fullfile('.\Data','Tree_*.mat'));
fileNames = natsortfiles(fileNames);

outputMatrix = [];
for i = 1:1:size(fileNames,1)
    fileName = fullfile('.\Data',fileNames(i).name);
    load(fileName);
    % Calculate characteristic a
    selectIDs = find([treeModel(:).IsCredible]);
    radii = [treeModel(selectIDs).BranchRadius];
    minValue = min(radii);
    maxValue = max(radii);
    breaks = linspace(minValue,maxValue,11);
    sampleMatrix = [];
    for j = 1:1:length(breaks)-1
        tempValue1 = breaks(j);
        tempValue2 = breaks(j+1);
        tempValue = (tempValue1 + tempValue2)/2;
        selectIDs = find((radii >= tempValue1) & (radii <= tempValue2));
        if(length(selectIDs) == 0)
            continue;
        end
        tempRelativeFreq = length(selectIDs)/length(radii)/(tempValue2-tempValue1);
        tempMatrix = [tempValue,tempRelativeFreq];
        sampleMatrix = [sampleMatrix;tempMatrix];
    end
    x = log(sampleMatrix(:,1));
    y = log(sampleMatrix(:,2));
    mx = mean(x);
    my = mean(y);
    sx = std(x);
    sy = std(y);
    r = corr(x,y);
    slope = sign(r)*sy/sx;
    characteristicA = -1/slope;
    % Calculate characteristic b
    lengths = [treeModel(:).BranchLength];
    minValue = min(lengths);
    maxValue = max(lengths);
    breaks = linspace(minValue,maxValue,11);
    sampleMatrix = [];
    for j = 1:1:length(breaks)-1
        tempValue1 = breaks(j);
        tempValue2 = breaks(j+1);
        tempValue = (tempValue1 + tempValue2)/2;
        selectIDs = find((lengths >= tempValue1) & (lengths <= tempValue2));
        if(length(selectIDs) == 0)
            continue;
        end
        tempRelativeFreq = length(selectIDs)/length(lengths)/(tempValue2-tempValue1);
        tempMatrix = [tempValue,tempRelativeFreq];
        sampleMatrix = [sampleMatrix;tempMatrix];
    end
    x = log(sampleMatrix(:,1));
    y = log(sampleMatrix(:,2));
    mx = mean(x);
    my = mean(y);
    sx = std(x);
    sy = std(y);
    r = corr(x,y);
    slope = sign(r)*sy/sx;
    characteristicB = -1/slope;
    % Output
    tempMatrix = [i,speciesList(i,2),characteristicA,characteristicB];
    outputMatrix = [outputMatrix;tempMatrix];
    disp(i);
end

% Save
outputCell = {'TreeID','SpeciesID','A','B'};
outputCell = [outputCell;num2cell(outputMatrix)];
outputFileName = 'TreeLevelAB_Distribution.xlsx';
writecell(outputCell,outputFileName);