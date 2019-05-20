import * as React from 'react';
import classnames from 'classnames';
import PerfectScrollbar from 'perfect-scrollbar';

interface Props {
  children: React.ReactElement,
  className?: string,
}

class ScrollContainer extends React.PureComponent<Props> {
  ps: PerfectScrollbar;
  ref = React.createRef<HTMLElement>();

  componentDidMount = async () => {
    if (this.ref && this.ref.current) {
      this.ps = new PerfectScrollbar(this.ref.current);
    }
  }

  componentDidUpdate() {
    this.ps.update();
  }

  componentWillUnmount() {
    this.ps.destroy();
    delete this.ps;
  }

  render() {
    const { className, children } = this.props;
    return React.cloneElement(React.Children.only(children), {
      ref: this.ref,
      className: classnames(
        children.props.className,
        className,
        'scrollbar-container',
      ),
    });
  }
}

export default ScrollContainer;
