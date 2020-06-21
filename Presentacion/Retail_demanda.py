# To add a new cell, type '#%%'
# To add a new markdown cell, type '#%% [markdown]'
#%% Change working directory from the workspace root to the ipynb file location. Turn this addition off with the DataScience.changeDirOnImportExport setting
# ms-python.python added
import os
try:
	os.chdir(os.path.join(os.getcwd(), 'Proyectos/demanda2'))
	print(os.getcwd())
except:
	pass
#%%
from IPython import get_ipython

#%%
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import numpy as np
from stldecompose import decompose
from statsmodels.tsa import arima_model

get_ipython().run_line_magic('matplotlib', 'inline')
sns.set(rc={'figure.figsize':(25,10)})

#sns.set(rc={'figure.figsize':(25,10)})


#%%
sns.set(rc={'figure.figsize':(25,10)})


#%%
pd.to_datetime('2018-10-20 22:16:16')


#%%
pd.to_datetime('2018-10-20T22:16:16Z') #ISO 8601


#%%
pd.to_datetime('2018-10-20 22:16:16',utc=True)


#%%
pd.to_datetime('31/7/2007') #dd/mm/yy


#%%
pd.to_datetime('7/31/2007') #dd/mm/yy


#%%
pd.to_datetime('7/31/2007', dayfirst=True) #dd/mm/yy


#%%
pd.date_range(start='2019/01/01', freq='1B', periods=26)


#%%
# D es daily, B es business daily, M month, Q quaterly, etc.


#%%
pd.timedelta_range(start='1 day', end= '5 day', periods=6)


#%%
pd.timedelta_range(start='1 day', periods=6, freq='H')


#%%
data = pd.read_excel('muestra.xls')


#%%
data.head()


#%%
data.info()


#%%
data.describe()


#%%
# Drop rows with missing values and drop duplicate
data.dropna(inplace=True)
data.drop_duplicates(inplace=True)

# Visualize pairplot of df
sns.pairplot(data, hue='Segmento');


#%%



#%%
data = data.set_index('Fecha del pedido')
data = data.sort_index()
data.head()


#%%
data['Anio'] = data.index.year
data['Mes'] = data.index.month
data['Semana'] = data.index.week
data['Dia_semana'] = data.index.day_name()


#%%
data['Dia_semana'].value_counts()


#%%
data.head()


#%%
data['Total'].plot(linewidth=0.5)


#%%
df_g = data.groupby(["Fecha del pedido","Semana"])
df_g_sum = data.groupby(["Fecha del pedido","Semana"]).sum()


#%%
df_g_sum.head()


#%%
df_g_dem = df_g_sum['Cantidad']
np.log(df_g_dem).plot()


#%%
data.head()


#%%
data['Fecha del pedido'] = pd.to_datetime(data['Fecha del pedido']) - pd.to_timedelta(7, unit='d')


#%%
data1 = data.groupby(['Id. del producto', pd.Grouper(key='Fecha del pedido', freq='W-MON')])['Cantidad'].sum().reset_index().sort_values('Fecha del pedido')
print (data1)


#%%
data2 = data.groupby([pd.Grouper(key='Fecha del pedido', freq='W-MON')])['Cantidad'].sum().reset_index().sort_values('Fecha del pedido')
print (data2)


#%%
data2 = data2.set_index('Fecha del pedido')
data2 = data2.sort_index()
data2.head()


#%%
np.log(data2).plot()


#%%
stl = decompose(np.log(data2), period=52, lo_frac=0.2)


#%%
stl


#%%
sa_act = (stl.resid + stl.trend)
adj_pv = stl.seasonal.Cantidad.values[:26]
adj_oi = stl.seasonal.Cantidad.values[27:53]


#%%
stl.trend


#%%
sa_act.plot()


#%%
sa_act.to_csv("data_retail.csv")


#%%
cv_arima = arima_model.ARIMA(sa_act, order=(2,1,1), freq='W-MON')
cv_fit = cv_arima.fit()


#%%
# Esto debe cambiarse al inicio de cada temporada
season_start_date = pd.Timestamp('2018-12-31')


#%%
n_semanas_pasaron = int((act.index.max() - season_start_date).days/7)


#%%
n_preds = 26


#%%
forecast = cv_fit.forecast(n_preds)


#%%
forecast[0]


#%%
act_pred = (
    pd.DataFrame({'unadjusted_mean':forecast[0], 
              'error':forecast[1], 
              'lower_ci':forecast[2][:,0],
             'upper_ci':forecast[2][:,1],
             'estacionalidad':adj_pv[-n_preds:]})
            .assign(adjusted_mean = lambda x: x.estacionalidad + x.unadjusted_mean)
)


#%%



#%%
import pandas as pd
from pmprophet.model import PMProphet, Sampler


#%%
#act = pd.read_csv("act_nn.csv")
#df = df.head(180)
df = data2


#%%
# Fit both growth and intercept
m = PMProphet(df, growth=True, intercept=True, n_changepoints=20, changepoints_prior_scale=.001, name='model')

# Add monthly seasonality (order: 3)
m.add_seasonality(seasonality=52, fourier_order=3)


#%%
# Add weekly seasonality (order: 3)
m.add_seasonality(seasonality=26, fourier_order=3)


#%%
# Fit the model (using NUTS)
m.fit(method=Sampler.NUTS)


#%%
ddf = m.predict(26, alpha=0.2, include_history=True, plot=True)
m.plot_components(intercept=False)


#%%
from fbprophet import Prophet


#%%
df = pd.read_csv('data_retail.csv')


#%%
# Python
m = Prophet(yearly_seasonality=True)
m.fit(df)


#%%
future = m.make_future_dataframe(periods=26,freq = 'w')
future.tail()


#%%
forecast = m.predict(future)
forecast[['ds', 'yhat', 'yhat_lower', 'yhat_upper']].head()


#%%
fig1 = m.plot(forecast)


#%%
fig2 = m.plot_components(forecast)


#%%
from fbprophet.plot import plot_plotly
import plotly.offline as py
py.init_notebook_mode()

fig = plot_plotly(m, forecast)  # This returns a plotly Figure
py.iplot(fig)


#%%
import plotly.graph_objs as go


#%%



#%%
data2['Mes'] = data2.index.month
data2['Semana'] = data2.index.week
data2['Dia_semana'] = data2.index.day_name()


#%%
plt.scatter(df_rat_by_year.index, df_rat_by_year)
plt.xlabel('year of release')
plt.ylabel('median rating');


#%%
df_g = data.groupby("Fecha del pedido")['Ganancia'].sum()
df_g = pd.DataFrame({'Ganancia':df_g})
df_g['Ganancia'].plot(linewidth=0.6)


#%%
data1 = data.Anio.isin( ['2016','2017','2018'])
data1.head()


#%%
df = data1.groupby("Fecha del pedido")
df_cantidad = pd.DataFrame({'Cantidad':df})
#df_cantidad=df_cantidad.Anio.isin( ['2016','2017','2018'])
#df_cantidad.head()
df_cantidad['Cantidad'].plot(linewidth=0.55)


#%%
df_cant2 = data.groupby(["Fecha del pedido","Semana"])[['Cantidad']].sum()


#%%
df_cant2.head


#%%
df_cantidad.loc['2017','Cantidad'].plot() #df_cantidad['Cantidad'].plot(linewidth=0.6)


#%%
#data['Semana'] = data.index.week
data.loc['2017', ['Semana','Cantidad']].groupby(['Semana']).sum().plot()


#%%
data.head()


#%%
mu_dia = data.sort_values('Dia_semana').loc['2018',['Dia_semana','Cantidad']].reset_index().groupby('Dia_semana',as_index=False).mean()
mu_dia


#%%
mu_dia.sort_index(axis=0).plot()


#%%
from pandas.plotting import autocorrelation_plot


#%%
df_cantidad.head()


#%%
autocorrelation_plot(df_cantidad.Cantidad)


#%%
fig, ax= plt.subplots();
autocorrelation_plot(df_cantidad.Cantidad)
ax.set_xlim(0,26);


#%%
from bokeh.io import output_notebook
output_notebook()


#%%
45


#%%



