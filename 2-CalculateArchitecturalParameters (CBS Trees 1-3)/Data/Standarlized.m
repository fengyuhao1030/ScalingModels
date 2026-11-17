clc,clear;

speciesList = xlsread('SpeciesList.xlsx',1);
fileNames = dir(fullfile('.','Original_*.mat'));
fileNames = natsortfiles(fileNames);

for i = 1:1:size(fileNames,1)
    fileName = fullfile('.',fileNames(i).name);
    load(fileName);
    treeModel = struct( ...
        'BranchLabel',{MST.WBELabel},...
        'BranchOrder',{MST.WBEOrder},...
        'BranchRadius',{MST.WBERadius},...
        'BranchLength',{MST.WBELength},...
        'SkeletonPointIDs',{MST.SkeletonIDs},...
        'SkeletonCurve',{MST.Curve},...
        'IsTerminal',{MST.IsEnd},...
        'IsCredible',{MST.IsWBECredible});
    for j = 1:1:size(treeModel,2)
        treeModel(j).BranchOrder = treeModel(j).BranchOrder - 1;
    end
    [folder,name,ext] = fileparts(fileName);
    name = erase(name,'Original_');
    outputFileName = fullfile(folder,[name,ext]);
    save(outputFileName,'treeModel');
    disp(num2str(i));
end