clc,clear;

speciesList = xlsread('.\Data\SpeciesList.xlsx',1);

fileNames = dir(fullfile('.\Data','Tree_*.mat'));
fileNames = natsortfiles(fileNames);

outputMatrix = [];
for i = 1:1:size(fileNames,1)
    fileName = fullfile('.\Data',fileNames(i).name);
    load(fileName);
    parentBranchRadiusSum = 0;
    uParentBranchLengthSum = 0;
    vParentBranchLengthSum = 0;
    uChildBranchRadiusSum = 0;
    uChildBranchLengthSum = 0;
    vChildBranchRadiusSum = 0;
    vChildBranchLengthSum = 0;
    for j = 1:1:size(treeModel,2)
        % Step 1: 基本信息，判断是否进行统计
        if treeModel(j).IsTerminal
            continue;
        end
        parentBranchID = j;
        parentBranchRadius = treeModel(parentBranchID).BranchRadius;
        childBranchIDs = [];
        childBranchRadii = [];
        for k = 1:1:size(treeModel,2)
            if contains(treeModel(k).BranchLabel,treeModel(parentBranchID).BranchLabel) && (treeModel(k).BranchOrder == treeModel(parentBranchID).BranchOrder+1)
                childBranchIDs = [childBranchIDs,k];
                childBranchRadii = [childBranchRadii,treeModel(k).BranchRadius];
            end
        end
        if (max(childBranchRadii) >= parentBranchRadius) || (treeModel(parentBranchID).IsCredible == false) || all([treeModel(childBranchIDs).IsCredible] == false)
            continue;
        end
        if length(childBranchIDs) > 2
            continue;
        end
        % Step 2: 分配u子枝，v子枝
        if treeModel(childBranchIDs(1)).BranchRadius >= treeModel(childBranchIDs(2)).BranchRadius
            uChildBranchID = childBranchIDs(1);
            vChildBranchID = childBranchIDs(2);
        else
            uChildBranchID = childBranchIDs(2);
            vChildBranchID = childBranchIDs(1);
        end
        % Step 3：处理u枝，生成当前u枝的u生成树
        uChildBranchRadius = treeModel(uChildBranchID).BranchRadius;
        tempTreeModel = treeModel(uChildBranchID);
        tempBranchID = uChildBranchID;
        while true
            if treeModel(tempBranchID).IsTerminal
                break;
            end
            maxChildRadius = -1;
            maxChildBranchID = 0;
            for k = 1:1:size(treeModel,2)
                if contains(treeModel(k).BranchLabel,treeModel(tempBranchID).BranchLabel) && (treeModel(k).BranchOrder == treeModel(tempBranchID).BranchOrder+1)
                    if treeModel(k).BranchRadius > maxChildRadius
                        maxChildBranchID = k;
                        maxChildRadius = treeModel(k).BranchRadius;
                    end
                end
            end
            tempTreeModel = [tempTreeModel,treeModel(maxChildBranchID)];
            tempBranchID = maxChildBranchID;
        end
        [value,index] = sort([tempTreeModel(:).BranchOrder]);
        tempTreeModel = tempTreeModel(index);
        % Step 3.1: 生成近邻表
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
        % Step 3.2: 生成图并查找最长最短路径
        treeGraph = graph(neighborMatrix(:,1),neighborMatrix(:,2),neighborMatrix(:,3));
        uChildBranchLength = 0;
        uChildLongestPath = [];
        for k = 1:1:length(sourceIDs)
            [Path,distance] = shortestpath(treeGraph,sourceIDs(k),rootID);
            if distance > uChildBranchLength
                uChildBranchLength = distance;
                uChildLongestPath = Path;
            end
        end
        % Step 3.3：处理uParentBranchLength
        uParentBranchLength = uChildBranchLength + treeModel(parentBranchID).BranchLength;
        % Step 4：处理v枝，生成当前v枝的v生成树
        vChildBranchRadius = treeModel(vChildBranchID).BranchRadius;
        tempTreeModel = treeModel(vChildBranchID);
        tempBranchID = vChildBranchID;
        while true
            if treeModel(tempBranchID).IsTerminal
                break;
            end
            minChildRadius = 9999;
            minChildBranchID = 0;
            for k = 1:1:size(treeModel,2)
                if contains(treeModel(k).BranchLabel,treeModel(tempBranchID).BranchLabel) && (treeModel(k).BranchOrder == treeModel(tempBranchID).BranchOrder+1)
                    if treeModel(k).BranchRadius < minChildRadius
                        minChildBranchID = k;
                        minChildRadius = treeModel(k).BranchRadius;
                    end
                end
            end
            tempTreeModel = [tempTreeModel,treeModel(minChildBranchID)];
            tempBranchID = minChildBranchID;
        end
        [value,index] = sort([tempTreeModel(:).BranchOrder]);
        tempTreeModel = tempTreeModel(index);
        % Step 4.1: 生成近邻表
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
        % Step 4.2: 生成图并查找最长最短路径
        treeGraph = graph(neighborMatrix(:,1),neighborMatrix(:,2),neighborMatrix(:,3));
        vChildBranchLength = 0;
        vChildLongestPath = [];
        for k = 1:1:length(sourceIDs)
            [Path,distance] = shortestpath(treeGraph,sourceIDs(k),rootID);
            if distance > vChildBranchLength
                vChildBranchLength = distance;
                vChildLongestPath = Path;
            end
        end
        % Step 4.3：处理vParentBranchLength
        vParentBranchLength = vChildBranchLength + treeModel(parentBranchID).BranchLength;
        % Step 5: 加和
        parentBranchRadiusSum = parentBranchRadiusSum + parentBranchRadius;
        uChildBranchRadiusSum = uChildBranchRadiusSum + uChildBranchRadius;
        vChildBranchRadiusSum = vChildBranchRadiusSum + vChildBranchRadius;
        uPathDepth = 0;
        for k = 1:1:size(treeModel,2)
            if any(ismember(treeModel(k).SkeletonPointIDs,uChildLongestPath))
                uPathDepth = uPathDepth + 1;
            end
        end
        if uPathDepth >= 5
            uParentBranchLengthSum = uParentBranchLengthSum + uParentBranchLength;
            uChildBranchLengthSum = uChildBranchLengthSum + uChildBranchLength;
        end
        vPathDepth = 0;
        for k = 1:1:size(treeModel,2)
            if any(ismember(treeModel(k).SkeletonPointIDs,vChildLongestPath))
                vPathDepth = vPathDepth + 1;
            end
        end
        if vPathDepth >= 5
            vParentBranchLengthSum = vParentBranchLengthSum + vParentBranchLength;
            vChildBranchLengthSum = vChildBranchLengthSum + vChildBranchLength;
        end
    end
    % Step 6: 计算specific ratios
    uBeta = uChildBranchRadiusSum/parentBranchRadiusSum;
    vBeta = vChildBranchRadiusSum/parentBranchRadiusSum;
    uGamma = uChildBranchLengthSum/uParentBranchLengthSum;
    vGamma = vChildBranchLengthSum/vParentBranchLengthSum;
    tempMatrix = [i,speciesList(i,2),uBeta,vBeta,uGamma,vGamma];
    outputMatrix = [outputMatrix;tempMatrix];
    disp(i);
end
outputCell = {'TreeID','SpeciesID','uBeta','vBeta','uGamma','vGamma'};
outputCell = [outputCell;num2cell(outputMatrix)];
outputFileName = 'SpecificRatios.xlsx';
writecell(outputCell,outputFileName);