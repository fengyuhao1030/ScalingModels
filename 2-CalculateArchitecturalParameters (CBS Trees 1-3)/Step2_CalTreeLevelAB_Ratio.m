clc,clear;

speciesList = xlsread('.\Data\SpeciesList.xlsx',1);

fileNames = dir(fullfile('.\Data','Tree_*.mat'));
fileNames = natsortfiles(fileNames);

outputMatrix = [];
for i = 1:1:size(fileNames,1)
    fileName = fullfile('.\Data',fileNames(i).name);
    load(fileName);
    radiusRatios = [];
    parentBranchNum1 = 0;
    childBranchNum1 = 0;
    lengthRatios = [];
    parentBranchNum2 = 0;
    childBranchNum2 = 0;
    for j = 1:1:size(treeModel,2)
        if treeModel(j).IsTerminal
            continue;
        end
        tempLabel = treeModel(j).BranchLabel;
        tempOrder = treeModel(j).BranchOrder;
        tempRadius = treeModel(j).BranchRadius;
        treeLength = treeModel(j).BranchLength;
        parentBranchID = j;
        childBranchIDs = [];
        for k = 1:1:size(treeModel,2)
            if contains(treeModel(k).BranchLabel,tempLabel) && (treeModel(k).BranchOrder == tempOrder+1)
                childBranchIDs = [childBranchIDs,k];
            end
        end
        % Calculate characteristic a
        for k = 1:1:length(childBranchIDs)
            parentRadius = treeModel(parentBranchID).BranchRadius;
            childRadius = treeModel(childBranchIDs(k)).BranchRadius;
            tempIsCredible = treeModel(childBranchIDs(k)).IsCredible;
            if tempIsCredible && (parentRadius > 0) && (childRadius > 0)
                radiusRatios = [radiusRatios,childRadius/parentRadius];
                parentBranchNum1 = parentBranchNum1 + 1;
                childBranchNum1 = childBranchNum1 + length(childBranchIDs);
            end
        end
        % Calculate characteristic b
        for k = 1:1:length(childBranchIDs)
            parentLength = treeModel(parentBranchID).BranchLength;
            childLength = treeModel(childBranchIDs(k)).BranchLength;
            if childLength < parentLength
                lengthRatios = [lengthRatios,childLength/parentLength];
                parentBranchNum2 = parentBranchNum2 + 1;
                childBranchNum2 = childBranchNum2 + length(childBranchIDs);
            end
        end
    end
    meanRadiusRatio = mean(radiusRatios);
    meanLengthRatio = mean(lengthRatios);
    characteristicA = -log(meanRadiusRatio)/log(childBranchNum1/parentBranchNum1);
    characteristicB = -log(meanLengthRatio)/log(childBranchNum2/parentBranchNum2);
    % Output
    tempMatrix = [i,speciesList(i,2),characteristicA,characteristicB];
    outputMatrix = [outputMatrix;tempMatrix];
    disp(i);
end

% Save
outputCell = {'TreeID','SpeciesID','A','B'};
outputCell = [outputCell;num2cell(outputMatrix)];
outputFileName = 'TreeLevelAB_Ratio.xlsx';
writecell(outputCell,outputFileName);