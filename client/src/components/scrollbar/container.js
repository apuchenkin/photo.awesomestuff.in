import React from 'react';
import classnames from 'classnames';
import { element, string } from 'prop-types';

const PerfectScrollbar = isBrowser ? require('perfect-scrollbar').default : null;

class ScrollContainer extends React.PureComponent {
  componentDidMount() {
    this.ps = new PerfectScrollbar(this.root);
  }

  componentDidUpdate() {
    this.ps.update();
  }

  componentWillUnmount() {
    this.ps.destroy();
    this.ps = null;
  }

  render() {
    const { className, children } = this.props;
    return React.cloneElement(React.Children.only(children), {
      ref: ((cmp) => { this.root = cmp; }),
      className: classnames(
        className,
        'scrollbar-container',
      ),
    });
  }
}

ScrollContainer.propTypes = {
  children: element.isRequired,
  className: string,
};

ScrollContainer.defaultProps = {
  className: null,
};

export default ScrollContainer;
