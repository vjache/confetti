# !/bin/sh
echo "1. Get entire effective config for context 'test/app_1':"
curl "http://localhost:8081/conf/test/app_1"
echo ""
echo ""

echo "2. Get entire effective config for deeper context 'test/app_1/some.particular.host':"
curl "http://localhost:8081/conf/test/app_1/some.particular.host"
echo ""
echo ""

echo "3. Get variable 'level' value for a subject 'logging' at context 'test/app_1/some.particular.host':"
curl "http://localhost:8081/conf/test/app_1/some.particular.host?subject=logging&variables=level"
echo ""
echo ""

echo "4. Get variables 'metrics_server', 'message_server' values for subject 'external_services' at context 'test/app_1/some.particular.host':"
curl "http://localhost:8081/conf/test/app_1/some.particular.host?subject=external_services&variables=metrics_server,message_server"
echo ""
echo ""

echo "5. Get entire effective config for a several subjects 'external_services', 'logging' at context 'test/app_1':"
curl "http://localhost:8081/conf/test/app_1?subjects=external_services,logging"
echo ""
echo ""
