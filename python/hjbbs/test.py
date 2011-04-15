# -*- coding: utf-8 -*-
# test.py --- Time-stamp: <2010-07-08 12:51:31 Thursday by julian>
# Copyright 2010 Julian Qian
# Author: julian@PDDES.cdc.veritas.com
# Version: $Id: test.py,v 0.0 2010/07/08 04:50:26 julian Exp $
# Keywords: 

from log import logger

logger.debug('Test Debug a = %s', 1)
logger.error('Test Error!')
logger.info('info here')


def main():
    "test.py"
    
if __name__ == "__main__":
    main()    
