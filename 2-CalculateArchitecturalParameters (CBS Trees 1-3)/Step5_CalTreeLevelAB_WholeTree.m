clc,clear;

speciesList = xlsread('.\Data\SpeciesList.xlsx',1);

fileNames = dir(fullfile('.\Data','Tree_*.mat'));
fileNames = natsortfiles(fileNames);

outputMatrix = [];
for i = 1:1:size(fileNames,1)
    fileName = fullfile('.\Data',fileNames(i).name);
    load(fileName);
    parentBranchRadiusSum = 0;
    parentBranchLengthSum = 0;
    childBranchRadiusSum = 0;
    childBranchLengthSum = 0;
    childBranchNumSum = 0;
    parentBranchNumSum = 0;
    for j = 1:1:size(treeModel,2)
        if treeModel(j).IsTerminal
            continue;
        end
        parentBranchID = j;
        parentBranchRadius = treeModel(j).BranchRadius;
        childBranchIDs = [];
        childBranchRadii = [];
        for k = 1:1:size(treeModel,2)
            if contains(treeModel(k).BranchLabel,treeModel(parentBranchID).BranchLabel) && (treeModel(k).BranchOrder == treeModel(parentBranchID).BranchOrder+1)
                childBranchIDs = [childBranchIDs,k];
                childBranchRadii = [childBranchRadii,treeModel(k).BranchRadius];
            end
        end
        if (max(childBranchRadii) >= parentBranchRadius) || (treeModel(parentBranchID).IsCredible == 0) || all([treeModel(childBranchIDs).IsCredible] == false)
            continue;
        end
        %% Step 1: 获取父树的结构参数值
        selectIDs = [];
        for k = 1:1:size(treeModel,2)
            if contains(treeModel(k).BranchLabel,treeModel(parentBranchID).BranchLabel)
                selectIDs = [selectIDs,k];
            end
        end
        tempTreeModel = treeModel(selectIDs);
        [value,index] = sort([tempTreeModel(:).BranchOrder]);
        tempTreeModel = tempTreeModel(index);
        % Step 1.1: 生成近邻表
        neighborMatrix = [];
        for k = 1:1:size(tempTreeModel,2)
            for l = 1:1:size(tempTreeModel(k).SkeletonCurve,1)-1
                pointID_1 = tempTreeModel(k).SkeletonPointIDs(l);
                pointID_2 = tempTreeModel(k).SkeletonPointIDs(l+1);
                point_1 = tempTreeModel(k).SkeletonCurve(l,:);
                point_2 = tempTreeModel(k).SkeletonCurve(l+1,:);
                distance = sqrt(sum((point_1 - point_2).^2));
                neighborMatrix = [neighborMatrix;[pointID_1,pointID_2,distance]];
            end
        end
        rootID = tempTreeModel(1).SkeletonPointIDs(1);
        sourceIDs = [];
        for k = 1:1:size(tempTreeModel,2)
            sourceIDs = [sourceIDs,tempTreeModel(k).SkeletonPointIDs];
        end
        sourceIDs = sort(unique(sourceIDs));
        delID = find(sourceIDs == rootID);
        sourceIDs(delID) = [];
        % Step 1.2: 生成图并查找最短路径
        treeGraph = graph(neighborMatrix(:,1),neighborMatrix(:,2),neighborMatrix(:,3));
        parentBranchLength = 0;
        for k = 1:1:length(sourceIDs)
            [Path,distance] = shortestpath(treeGraph,sourceIDs(k),rootID);
            if distance > parentBranchLength
                parentBranchLength = distance;
            end
        end
        %% Step 2: 获取子树的结构参数值
        for k = 1:1:length(childBranchIDs)
            if treeModel(childBranchIDs(k)).IsCredible
                childBranchID = childBranchIDs(k);
                childBranchRadius = childBranchRadii(k);
                selectIDs = [];
                for l = 1:1:size(treeModel,2)
                    if contains(treeModel(l).BranchLabel,treeModel(childBranchID).BranchLabel)
                        selectIDs = [selectIDs,l];
                    end
                end
                tempTreeModel = treeModel(selectIDs);
                [value,index] = sort([tempTreeModel(:).BranchOrder]);
                tempTreeModel = tempTreeModel(index);
                % Step 2.1: 生成近邻表
                neighborMatrix = [];
                for l = 1:1:size(tempTreeModel,2)
                    for m = 1:1:size(tempTreeModel(l).SkeletonCurve,1)-1
                        pointID_1 = tempTreeModel(l).SkeletonPointIDs(m);
                        pointID_2 = tempTreeModel(l).SkeletonPointIDs(m+1);
                        point_1 = tempTreeModel(l).SkeletonCurve(m,:);
                        point_2 = tempTreeModel(l).SkeletonCurve(m+1,:);
                        distance = sqrt(sum((point_1 - point_2).^2));
                        neighborMatrix = [neighborMatrix;[pointID_1,pointID_2,distance]];
                    end
                end
                rootID = tempTreeModel(1).SkeletonPointIDs(1);
                sourceIDs = [];
                for l = 1:1:size(tempTreeModel,2)
                    sourceIDs = [sourceIDs,tempTreeModel(l).SkeletonPointIDs];
                end
                sourceIDs = sort(unique(sourceIDs));
                delID = find(sourceIDs == rootID);
                sourceIDs(delID) = [];
                % Step 2.2: 生成图并查找最短路径
                treeGraph = graph(neighborMatrix(:,1),neighborMatrix(:,2),neighborMatrix(:,3));
                childBranchLength = 0;
                childLongestPath = [];
                for l = 1:1:length(sourceIDs)
                    [Path,distance] = shortestpath(treeGraph,sourceIDs(l),rootID);
                    if distance > childBranchLength
                        childBranchLength = distance;
                        childLongestPath = Path;
                    end
                end
                %% Step 3: 输出
                childBranchRadiusSum = childBranchRadiusSum + childBranchRadius;
                parentBranchRadiusSum = parentBranchRadiusSum + parentBranchRadius;
                pathDepth = 0;
                for l = 1:1:size(treeModel,2)
                    if any(ismember(treeModel(l).SkeletonPointIDs,childLongestPath))
                        pathDepth = pathDepth + 1;
                    end
                end
                if pathDepth >= 5
                    childBranchLengthSum = childBranchLengthSum + childBranchLength;
                    parentBranchLengthSum = parentBranchLengthSum + parentBranchLength;
                end
            end
        end
        childBranchNumSum = childBranchNumSum + length(childBranchIDs);
        parentBranchNumSum = parentBranchNumSum + 1;
    end
    characteristicA = log(parentBranchRadiusSum/childBranchRadiusSum)/log(childBranchNumSum/parentBranchNumSum);
    characteristicB = log(parentBranchLengthSum/childBranchLengthSum)/log(childBranchNumSum/parentBranchNumSum);
    % Output
    tempMatrix = [i,speciesList(i,2),characteristicA,characteristicB];
    outputMatrix = [outputMatrix;tempMatrix];
    disp(i);
end

% Save
outputCell = {'TreeID','SpeciesID','A','B'};
outputCell = [outputCell;num2cell(outputMatrix)];
outputFileName = 'TreeLevelAB_WholeTree.xlsx';
writecell(outputCell,outputFileName);