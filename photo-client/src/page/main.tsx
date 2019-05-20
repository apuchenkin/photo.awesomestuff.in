import * as React from 'react';
import classnames from 'classnames';
import { Helmet } from 'react-helmet';
import Scrollbar from '@app/components/scrollbar';
import Footer from '@app/components/footer';
// @ts-ignore
import withStyles from 'isomorphic-style-loader/withStyles';
import style from './main.scss';

declare global {
  interface Window { gtag: any; }
}

interface Main {
  header: React.ReactNode;
  title: string;
  langs: Locale[];
  className?: string;
}

const Main: React.FunctionComponent<Main> = ({
  children,
  header,
  title,
  langs,
  className,
}) => (
  <>
    {header}
    <Scrollbar>
      <main
        className={classnames(
          style.main,
          className,
        )}
      >
        <Helmet
          titleTemplate={`%s - ${title}`}
          defaultTitle={title}
          onChangeClientState={(helmet) => {
            if (typeof window.gtag !== 'undefined') {
              window.gtag('event', 'page_view', {
                page_location: location.href,
                page_path: location.pathname,
                page_title: helmet.title,
              });
            }
          }}
        />

        {children}
        <Footer langs={langs} />
      </main>
    </Scrollbar>
  </>
);

export default withStyles(style)(Main);
