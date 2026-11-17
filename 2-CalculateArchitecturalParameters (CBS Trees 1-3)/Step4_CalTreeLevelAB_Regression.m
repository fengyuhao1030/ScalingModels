clc,clear;

speciesList = xlsread('.\Data\SpeciesList.xlsx',1);

fileNames = dir(fullfile('.\Data','Tree_*.mat'));
fileNames = natsortfiles(fileNames);

outputMatrix = [];
for i = 1:1:size(fileNames,1)
    fileName = fullfile('.\Data',fileNames(i).name);
    load(fileName);
    % Get analysis matrix
    analysisMatrix = [];
    for j = 1:1:size(treeModel,2)
        if treeModel(j).IsTerminal
            continue;
        end
        tempLabel = treeModel(j).BranchLabel;
        tempRadius = treeModel(j).BranchRadius;
        tempLength = treeModel(j).BranchLength;
        tempIsCredible = treeModel(j).IsCredible;
        tempTipNum = 0;
        for k = 1:1:size(treeModel,2)
            if contains(treeModel(k).BranchLabel,tempLabel) && treeModel(k).IsTerminal
                tempTipNum = tempTipNum + 1;
            end
        end
        tempMatrix = [tempRadius,tempLength,tempTipNum,tempIsCredible];
        analysisMatrix = [analysisMatrix;tempMatrix];
    end
    selectIDs = find(analysisMatrix(:,1) > 0);
    analysisMatrix = analysisMatrix(selectIDs,:);
    % Calculate characteristic a
    selectIDs = find(analysisMatrix(:,4) == true);
    x = log(analysisMatrix(selectIDs,3));
    y = log(analysisMatrix(selectIDs,1));
    mx = mean(x);
    my = mean(y);
    sx = std(x);
    sy = std(y);
    r = corr(x,y);
    slope = sign(r)*sy/sx;
    characteristicA = slope;
    % Calculate characteristic b
    x = log(analysisMatrix(:,3));
    y = log(analysisMatrix(:,2));
    mx = mean(x);
    my = mean(y);
    sx = std(x);
    sy = std(y);
    r = corr(x,y);
    slope = sign(r)*sy/sx;
    characteristicB = slope;
    % Output
    tempMatrix = [i,speciesList(i,2),characteristicA,characteristicB];
    outputMatrix = [outputMatrix;tempMatrix];
    disp(i);
end

% Save
outputCell = {'TreeID','SpeciesID','A','B'};
outputCell = [outputCell;num2cell(outputMatrix)];
outputFileName = 'TreeLevelAB_Regression.xlsx';
writecell(outputCell,outputFileName);