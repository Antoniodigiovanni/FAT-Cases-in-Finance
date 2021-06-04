clear;
Years = readmatrix('Years_list.csv');
Years = Years(:,2);
Years = sort(Years);




for i=1:length(Years)
    Year = Years(i);
    Year
    %Year=2008;
    % Add a try-catch as some years will not be present.
try
    cov_m = readtable(strcat('cov_',string(Year),'.csv'), 'ReadRowNames',true, 'ReadVariableNames', true); %csvread('cov_m.csv',1,1);
    Ret = readtable(strcat('Ret_',string(Year),'.csv'), 'ReadRowNames',true, 'ReadVariableNames', true);
    Ret = Ret(:,2);
    Ids = string(cell2mat(cov_m.Properties.RowNames));

cov_m = table2array(cov_m);
Ret = table2array(Ret);

%x = sdpvar(size(cov_m,1),1);
%Cons = sum(x) == 1;
%Cons = [Cons, x >= 0];
%Cons = [Cons, x <= 0.05];

a=size(cov_m);
a=sum(a)/2;
%x=NaN(a,1);




%Ops = sdpsettings ('solver','gurobi', '', 'verbose', 1);
%Res = optimize(Cons, -Obj, Ops);
fun = @(x)(-(transpose(x)*Ret)./sqrt((transpose(x)*cov_m*x)));
%years 2008,2014 need lower optimality tolerance due to large duration of
%calculation
options = optimoptions(@fmincon, 'MaxFunctionEvaluations',10000000, 'MaxIterations',100000,'OptimalityTolerance',0.00001,'StepTolerance', 1.0000e-6);


%fun=@SharpeRatio;
A=[];
b= [];
Aeq =ones(1,a);
beq =1;
lb = zeros(a,1);
ub = ones(a,1)*0.05;
x0_0=zeros(a,1);
x_0_0(1,:)=1;
x0 = x0_0;
nonlcon = @ncol;
x = fmincon(fun,x0,A,b,Aeq,beq,lb,ub,nonlcon, options);


x_vect = value(x);
x_vect(x_vect<0.0001)=0;
x_t = array2table(x_vect,'RowNames', Ids);
writetable(x_t, strcat('weights_',string(Year),'.csv'), 'Delimiter', ',', 'QuoteStrings',true, 'WriteRowNames', true);

catch ME
    warning('Error');
end
end


function [c,ceq] = ncol(x)
c =[];
ceq = [];
end