# Market-Emotion-Indicator






此程序为使用输入的数据进行训练进而预测一定时间的收益率

使用说明：

1、根据收益率类型将各项数据填入【区间最高收益率／区间最大亏损率／区间收益率.csv】文件中
2、将文件夹内【index_importance.rda】【区间最高收益率／区间最大亏损率／区间收益率.csv】【function_train&predict.R】3个文件复制到R的工作路径下
3、在R中键入 source(‘function_train&predict.R’)   
4、使用train_predict_rr函数来进行预测 train_predict_rr(dat, n, index, span, type) 其中
dat为数据集
n为训练集比例（默认为0.8）
index可选【沪深300】（默认）【万得全A】
span可选【1个月】（默认）【3个月】【6个月】【12个月】【24个月】【36个月】
type可选【区间最高收益率】（默认）【区间最大亏损率】【区间收益率】
函数返回结果为使用rpart、bagging、randomForest、gbm、xgboost5种模型所得到的预测结果


