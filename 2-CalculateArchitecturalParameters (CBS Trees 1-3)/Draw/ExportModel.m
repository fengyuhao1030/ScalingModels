% This File is part of the L1-Tree algorithm
% 
% L1-Tree is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
% 
% If you use this file, please cite the article entitled "L1-Tree: 
% A novel algorithm for constructing 3D tree models and estimating branch architectural 
% traits using terrestrial laser scanning data"

% ------------------------------------------------------------------------------
% L1TreeExportModel.m    The function for export the 3D tree model
%
% Version 1.0
% Latest update     05 Feb 2024
% 
% Copyright (C) 2024 Sulab, Institude of Botany, The Chinese Academy of Sciences
% If you have any questions about using the code, please contact Yuhao Feng (fengyuhao@pku.edu.cn)
%
% INPUTS:
% fileName     Filename for point cloud data without extensions
% treeModel    The 3D tree model
%
% OUTPUTS:
% outputFileName    The output filename
% ------------------------------------------------------------------------------

fileName = 'Tree_3.mat';
[folder,sortFileName,ext] = fileparts(fileName);
load(fileName);

maxWBEOrder = max([treeModel(:).BranchOrder]);
for i = 0:1:maxWBEOrder
    for j = 1:1:size(treeModel,2)
        if (treeModel(j).BranchOrder ~= i) || ~isnan(treeModel(j).BranchRadius)
            continue;
        end
        for k = 1:1:size(treeModel,2)
            if contains(treeModel(j).BranchLabel,treeModel(k).BranchLabel) && (treeModel(k).BranchOrder == treeModel(j).BranchOrder-1)
                parentBranchID = k;
            end
        end
        treeModel(j).WBERadius = treeModel(parentBranchID).BranchRadius*0.75;
    end
end

outputMatrix = [];
for i = 1:1:size(treeModel,2)
    tempBranch = treeModel(i);
    tempOutputMatrix = [ones(size(tempBranch.SkeletonCurve,1),1)*i,tempBranch.SkeletonCurve,ones(size(tempBranch.SkeletonCurve,1),1)*tempBranch.BranchOrder,...
                        ones(size(tempBranch.SkeletonCurve,1),1)*tempBranch.BranchRadius];
    outputMatrix = [outputMatrix;tempOutputMatrix];
end
outputFileName = [sortFileName,'_Draw.txt'];
writematrix(outputMatrix,outputFileName,'Delimiter',' ');