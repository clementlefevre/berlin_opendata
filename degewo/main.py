import logging
import time
import schedule
from service.send_email import send_notification
from service.degewo_json_to_db import retrieve_offers, write_new_offers_to_db, reformat_df
from config import EXCEL_FILE


logging.basicConfig(filename='degewo.log',level=logging.DEBUG,format='%(asctime)s:%(levelname)s:%(message)s' )


def job():
    try:
    
        df_online_offers = retrieve_offers()
        df_online_offers = reformat_df(df_online_offers)
        df_new_offers = write_new_offers_to_db(df_online_offers)
        if df_new_offers.shape[0]>0:
            send_notification(df_new_offers.url.tolist(),EXCEL_FILE)
    except Exception as e:
        logging.error(e)


schedule.every(10).minutes.do(job)

job()
while 1:
    schedule.run_pending()
    time.sleep(1)