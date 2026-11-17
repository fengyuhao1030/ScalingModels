clc,clear;

speciesList = xlsread('.\Data\SpeciesList.xlsx',1);

fileNames = dir(fullfile('.\Data','Tree_*.mat'));
fileNames = natsortfiles(fileNames);

outputMatrix = [];
for i = 1:1:size(fileNames,1)
    fileName = fullfile('.\Data',fileNames(i).name);
    load(fileName);
    characteristicAs = [];
    characteristicBs = [];
    for j = 1:1:size(treeModel,2)
        if treeModel(j).IsTerminal
            continue;
        end
        tempLabel = treeModel(j).BranchLabel;
        tempOrder = treeModel(j).BranchOrder;
        tempRadius = treeModel(j).BranchRadius;
        tempLength = treeModel(j).BranchLength;
        tempIsTerminal = treeModel(j).IsTerminal;
        parentBranchID = j;
        childBranchIDs = [];
        for k = 1:1:size(treeModel,2)
            if contains(treeModel(k).BranchLabel,tempLabel) && (treeModel(k).BranchOrder == tempOrder+1)
                childBranchIDs = [childBranchIDs,k];
            end
        end
        % Calculate characteristic a
        if all([treeModel(childBranchIDs).IsCredible])
            parentRadius = treeModel(parentBranchID).BranchRadius;
            childRadii = [treeModel(childBranchIDs).BranchRadius];
            fitValues = linspace(0,3,1000);
            diffValues = [];
            for k = 1:1:length(fitValues)
                fitValue = fitValues(k);
                diffValue = abs(parentRadius.^fitValue - sum(childRadii.^fitValue));
                diffValues = [diffValues,diffValue];
            end
            [minValue,minIndex] = min(diffValues);
            optFitValue = fitValues(minIndex);
            characteristicAs = [characteristicAs,1/optFitValue];
        end
        % Calculate characteristic b
        parentLength = treeModel(parentBranchID).BranchLength;
        childLengths = [treeModel(childBranchIDs).BranchLength];
        fitValues = linspace(-3,3,1000);
        diffValues = [];
        for k = 1:1:length(fitValues)
            fitValue = fitValues(k);
            diffValue = abs(parentLength.^fitValue - sum(childLengths.^fitValue));
            diffValues = [diffValues,diffValue];
        end
        [minValue,minIndex] = min(diffValues);
        optFitValue = fitValues(minIndex);
        if optFitValue > 0
            characteristicBs = [characteristicBs,1/optFitValue];
        end
    end
    characteristicA = mean(characteristicAs);
    characteristicB = mean(characteristicBs);
    % Output
    tempMatrix = [i,speciesList(i,2),characteristicA,characteristicB];
    outputMatrix = [outputMatrix;tempMatrix];
    disp(i);
end

% Save
outputCell = {'TreeID','SpeciesID','A','B'};
outputCell = [outputCell;num2cell(outputMatrix)];
outputFileName = 'TreeLevelAB_Conservation.xlsx';
writecell(outputCell,outputFileName);