%Predictor2 (DS)
close all
clear all
clc

load shoredata.mat
shoredata=table2array(shoredata);
Testdata=length(shoredata);
%%%%%
%KNN regression
PSL=2;
nn=4;

%Make a matrix with that lag
lag=lagmatrix(shoredata(:,1),0:PSL);

%make the prediction column
FS=shoredata(2:end,1);

%glue together shoreline matrix w/o:
%the bottom row ? there is no prediction shoreline
shoredatanew=[FS lag(1:end-1,:) shoredata(1:end-1,4)];

%Now remove the top 'PastShoreline' rows because there is no history
shoredatanew=shoredatanew((PSL+1):end,:);
%Catalog of past data
Catalog=shoredatanew(1:length(shoredatanew),2:end);


%%%%%%%%%
%load up the data to predict
%c1 year
%c2 month
%c3 day
%c4 shoreline
%c5 Hs
load Allwaves.mat

%make a matrix to hold predicitons
%make a matrix with S(t), S(t-1),S(t-2), Hs(t)
ToPredict=nan(length(Allwaves)-12784+1,7);
for i=1:PSL+1
    ToPredict(1,i)=Allwaves(12785-i,4);
end
ToPredict(:,(PSL+2))=Allwaves(12784:end,5);   %waves
ToPredict(:,(PSL+3))=Allwaves(12784:end,1);   %year
ToPredict(:,(PSL+4))=Allwaves(12784:end,2);   %month
ToPredict(:,(PSL+5))=Allwaves(12784:end,3);   %day


%%%%%%
%KNN regression

%one step ahead prediction with kNN
for i=1:1462
    %find Knn (L neighbors)
    [IDX,D]=knnsearch(Catalog,ToPredict(i,1:PSL+2),'K',nn,'Distance','seuclidean');
    
    %computed weighted average of K elements using IDW
    
    undo=size(IDX);
    vectorI=IDX(:);
    values=vectorI*0;
    for j=1:length(vectorI)
        %shoreline at t+1
        values(j)=shoredatanew(vectorI(j),1);
    end
    
    NNvals=reshape(values,undo);
    
    %Inverse distance wieghtinh..
    Weights=(1./D);
    ThePrediction=sum((Weights.*NNvals),2)./sum(Weights,2);
    %put the prediciton back into the matrix and shift the other values
    
    ToPredict(i+1,1)=ThePrediction;
    for k=1:PSL
        ToPredict(i+1,k+1)=ToPredict(i,k);
    end
end

plot(ToPredict(1:1462,1))


EBG1=[ToPredict(1:1462,5) ToPredict(1:1462,6) ToPredict(1:1462,7) ToPredict(1:1462,1) ToPredict(1:1462,4)];

save EBG1 EBG1