install.packages('rsconnect')

rsconnect::setAccountInfo(name='guga-nesh',
                          token='E9AA532FBC0812FD47FEFC0266E86DF5',
                          secret='EhesWf+qqxrpFv3eZksuJQ0HXz+MOHbq7J4ia4oJ')


library(rsconnect)
rsconnect::deployApp('C:/guga-nesh/IS415 Team 15 Proposal/regress_rangers_app')
