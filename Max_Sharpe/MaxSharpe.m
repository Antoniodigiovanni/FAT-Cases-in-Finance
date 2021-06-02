clear;
Years = readmatrix('Years_list.csv');
Years = Years(:,2);
for i=1:length(Years)
    Year = Years(i)
    % Add a try-catch as some years will not be present.
try
    cov_m = readtable(strcat('cov_',string(Year),'.csv'), 'ReadRowNames',true, 'ReadVariableNames', true); %csvread('cov_m.csv',1,1);
    Ret = readtable(strcat('Ret_',string(Year),'.csv'), 'ReadRowNames',true, 'ReadVariableNames', true);
    Ids = string(cell2mat(cov_m.Properties.RowNames));

cov_m = table2array(cov_m);
Ret = table2array(Ret);

x = sdpvar(size(cov_m,1),1);

Cons = sum(x) == 1;
Cons = [Cons, x >= 0];
Cons = [Cons, x <= 0.05];

Obj = x'*Ret/x'*cov_m*x;

Ops = sdpsettings ('solver','gurobi', 'verbose', 0);
Res = optimize(Cons, -Obj, Ops);

x_vect = value(x);
x_vect(x_vect<0.000009)=0;
x_t = array2table(x_vect,'RowNames', Ids);
writetable(x_t, strcat('weights_',string(Year),'.csv'), 'Delimiter', ',', 'QuoteStrings',true, 'WriteRowNames', true);
catch ME
    warning('Error');
end
end