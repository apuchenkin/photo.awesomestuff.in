import React from 'react';
import classnames from 'classnames';
import { element } from 'prop-types';
import withStyles from 'isomorphic-style-loader/lib/withStyles';
import baseStyle from '../style/style.less';
import style from '../style/main.less';

import Loader from '../components/loader';
import Footer from '../components/footer';

const Ps = isBrowser ? require('perfect-scrollbar') : null;

class Main extends React.PureComponent {
  componentDidMount() {
    Ps.initialize(this.content);
  }

  componentDidUpdate() {
    this.content.scrollTop = 0;
    Ps.update(this.content);
  }

  componentWillUnmount() {
    Ps.destroy(this.content);
  }

  render() {
    const { children, header } = this.props;

    return (
      <div
        className={classnames(
          style.main,
          style[header.type.className],
        )}
      >
        {header}
        <div className={style.content} ref={(c) => { this.content = c; }}>
          {children}
          <Footer />
        </div>
        {/* TODO: move loader */}
        <Loader key="loader" />
      </div>
    );
  }
}

Main.propTypes = {
  header: element.isRequired,
  children: element.isRequired,
};

export default withStyles(style, baseStyle)(Main);
